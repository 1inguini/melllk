{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module LLVMGen where

import           Definition                      as Def

import           Prelude                         as P

import           Control.Monad                   ((<=<), (>=>))
import qualified Control.Monad                   as M
import qualified Control.Monad.State             as St
import qualified Data.ByteString                 as BStr
import qualified Data.Either                     as E
import qualified Data.Function                   as Func
import qualified Data.List                       as Ls
import           Data.Map                        ((!?))
import qualified Data.Map                        as Map
import qualified Data.Maybe                      as May
import qualified Data.Text                       as T
import qualified Safe

import           LLVM.AST                        (Named ((:=)))
import qualified LLVM.AST                        as AST
import qualified LLVM.AST.AddrSpace              as AddrSpace
import qualified LLVM.AST.CallingConvention      as CallC
import qualified LLVM.AST.Constant               as Const
import qualified LLVM.AST.Float                  as Float
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Global                 as G
import qualified LLVM.AST.IntegerPredicate       as IP
import qualified LLVM.AST.Type                   as Type

import qualified LLVM
import qualified LLVM.Context                    as Context

import qualified LLVM.IRBuilder                  as IR

genLLVM :: AST.Module -> IO BStr.ByteString
genLLVM module' = Context.withContext $ \context ->
  LLVM.withModuleFromAST context module' LLVM.moduleLLVMAssembly

-- initialize type' valOp = do
--   unName <- unName
--   var <- alloca type' unName
--   store var 0 valOp

mangle :: AST.Name -> StateWithErr MetaData AST.Name
mangle name = do
  MetaData { names = names } <- St.get
  maybe
    (do { St.modify $ \meta -> meta { names = Map.insert name 1 names }
        ; pure name })
    (\num -> do { St.modify $ \meta -> meta { names = Map.insert name (num + 1) names }
                ; pure $ name <> nameShow num })
    $ names !? name

unName :: StateWithErr MetaData AST.Name
unName = do
  meta@MetaData { unusedNum = unusedNum } <- St.get
  St.put meta { unusedNum = unusedNum + 1 }
  pure $ AST.UnName unusedNum



-- Toplevels to Module

toplevels2module :: [Toplevel] -> StateWithErr MetaData AST.Module
toplevels2module exprs = do
  maybeDefs <- toplevel2def `mapM` exprs
  pure $ AST.defaultModule { AST.moduleDefinitions = fromMaybeList [] maybeDefs}

fromMaybeList accm ((Just a):listMay) = a:(fromMaybeList accm listMay)
fromMaybeList accm (Nothing:listMay)  = (fromMaybeList accm listMay)
fromMaybeList accm []                 = accm

-- Toplevel to Definition

toplevel2def :: Toplevel -> StateWithErr MetaData (Maybe AST.Definition)
toplevel2def top = do
  maybeGlobals <- toplevel2Global top
  pure $ AST.GlobalDefinition <$> maybeGlobals



-- TopLevel to Global

toplevel2Global :: Toplevel -> StateWithErr MetaData (Maybe AST.Global)
toplevel2Global (FuncDef name argNames bodyExpr) = do
  St.modify $ \meta -> meta { symbolTable = Map.fromList $
                              (\argName -> (argName, AST.LocalReference double argName))
                              <$> argNames }
  body <- exprs2BBlock [bodyExpr]
  St.modify $ \meta -> meta { symbolTable = Map.empty }
  defineFunc double name ((,) double <$> argNames) [body]


toplevel2Global (Extern name argNames) =
  externFunc double name ((,) double <$> argNames)

toplevel2Global (Expr expr) = do
  body <- exprs2BBlock [expr]
  defineFunc double (AST.Name "main") [] [body]



-- Exprs to Basicblock

exprs2BBlock :: [Expr] -> StateWithErr MetaData AST.BasicBlock
exprs2BBlock exprs = do
  term <- termRet =<< Safe.lastMay <$> mapM expr2operand exprs
  MetaData { namedInstrs = namedInstrs } <- St.get
  newBasicBlock namedInstrs term



-- Expr to Operand while appending namedInstrs

expr2operand :: Expr -> StateWithErr MetaData AST.Operand
expr2operand (Float num)   = doubleNum num
expr2operand (Var varName) = do
  MetaData { symbolTable = symbolTable } <- St.get
  May.maybe
    (throwErr $ "variable not assigned: " <> tShow varName)
    (\operand -> load double operand)
    $ symbolTable !? varName

expr2operand (BinOp op expr0 expr1) = do
  operand0 <- expr2operand expr0
  operand1 <- expr2operand expr1
  case op of
    Plus   -> fAdd operand0 operand1
    Minus  -> fSub operand0 operand1
    Times  -> fMul operand0 operand1
    Divide -> fDiv operand0 operand1

expr2operand (FuncCall name exprs) = do
  argOperands <- expr2operand `mapM` exprs
  MetaData { definitionTable = definitionTable } <- St.get
  May.maybe
    (throwErr $ "function not defined: " <> tShow name)
    (\operand -> call operand argOperands)
    $ definitionTable !? name




-- Global

defineFunc :: AST.Type -> AST.Name -> [(AST.Type, AST.Name)] -> [AST.BasicBlock]
           -> StateWithErr MetaData (Maybe AST.Global)
defineFunc type' name typedArgs basicBlocks = do
  addToDefinitionTable type' name typedArgs
  pure . Just $
    AST.functionDefaults { G.returnType = type'
                         , G.name = name
                         , G.parameters = (genParam <$> typedArgs, False)
                         , G.basicBlocks = basicBlocks }

externFunc :: AST.Type -> AST.Name -> [(AST.Type, AST.Name)]
           -> StateWithErr MetaData (Maybe AST.Global)
externFunc type' name typedArgs = do
  addToDefinitionTable type' name typedArgs
  pure Nothing

addToDefinitionTable :: AST.Type -> AST.Name -> [(AST.Type, AST.Name)]
                     -> StateWithErr MetaData ()
addToDefinitionTable type' name typedArgs =
  St.modify $ \meta ->
    meta { definitionTable =
           Map.insert name
           (AST.ConstantOperand $
            Const.GlobalReference
            AST.PointerType { pointerAddrSpace = AddrSpace.AddrSpace 0
                             , pointerReferent =
                               AST.FunctionType { resultType    = type'
                                                , argumentTypes = fst <$> typedArgs
                                                , isVarArg      = False }}
             name)
           $ definitionTable meta}
  -- St.modify $ \meta ->
  --   meta { definitionTable =
  --          Map.insert name
  --          (AST.LocalReference
  --           AST.FunctionType { resultType    = type'
  --                            , argumentTypes = fst <$> typedArgs
  --                            , isVarArg      = False}
  --            name)
  --          $ definitionTable meta}





-- Misc (Global)

genParam :: (AST.Type, AST.Name) -> AST.Parameter
genParam (type', name) = G.Parameter type' name []



-- Basicblock

newBasicBlock :: [AST.Named AST.Instruction] -> AST.Named AST.Terminator
           -> StateWithErr MetaData AST.BasicBlock
newBasicBlock namedInstrs terminator = do
  meta@MetaData { unusedNum = unusedNum } <- St.get
  St.put meta { unusedNum = unusedNum + 1 }
  pure $ AST.BasicBlock (AST.UnName unusedNum) namedInstrs terminator



-- append Named Instruction and return Operand

fAdd, fSub, fMul, fDiv :: AST.Operand -> AST.Operand
  -> StateWithErr MetaData AST.Operand
fAdd = fOps AST.FAdd
fSub = fOps AST.FSub
fMul = fOps AST.FMul
fDiv = fOps AST.FDiv

call :: AST.Operand -> [AST.Operand] -> StateWithErr MetaData AST.Operand
call fn args = do
  name <- unName
  St.modify $ \meta ->
    meta { namedInstrs = namedInstrs meta ++
           [name := AST.Call Nothing CallC.C [] (Right fn) ((\x -> (x, [])) <$> args) [] []] }
  pure $ AST.LocalReference double name

alloca :: AST.Type -> Maybe AST.Name -> StateWithErr MetaData AST.Operand
alloca type' (Just name) = mangle name >>= alloca' type'
alloca type' Nothing     = unName >>= alloca' type'

load :: AST.Type -> AST.Operand -> StateWithErr MetaData AST.Operand
load type' addr = do
  name <- unName
  let dest = AST.LocalReference type' name
  St.modify $ \meta -> meta { namedInstrs = namedInstrs meta ++
                              [name := AST.Load False addr Nothing 0 []] }
  pure dest


store :: AST.Operand -> AST.Operand -> StateWithErr MetaData ()
store addr val = St.modify $ \meta ->
    meta { namedInstrs = namedInstrs meta ++
           [AST.Do $ AST.Store False addr val Nothing 0 []] }


alloca' :: AST.Type -> AST.Name -> StateWithErr MetaData AST.Operand
alloca' type' name = do
  St.modify $ \meta -> meta { namedInstrs = namedInstrs meta ++
                              [name := AST.Alloca type' Nothing 0 []] }
  pure $ AST.LocalReference type' name

-- Misc (Named Instruction)

fOps :: (AST.FastMathFlags -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction)
     -> AST.Operand -> AST.Operand -> StateWithErr MetaData AST.Operand
fOps constr a b = do
  name <- unName
  St.modify $ \meta -> meta { namedInstrs = namedInstrs meta ++
                              [name := constr AST.noFastMathFlags a b []] }
  pure $ AST.LocalReference double name



-- Named Terminator

termRet :: Maybe AST.Operand -> StateWithErr MetaData (Named AST.Terminator)
termRet retOperand = pure . AST.Do $ AST.Ret retOperand []

termBr :: AST.Name -> StateWithErr MetaData (Named AST.Terminator)
termBr dest = pure . AST.Do $ AST.Br dest []

termCondbr :: AST.Operand -> AST.Name -> AST.Name -> StateWithErr MetaData (Named AST.Terminator)
termCondbr cond tr fl = pure . AST.Do $ AST.CondBr cond tr fl []



-- Operand

-- LocalReference Type Name
-- ConstantOperand Constant
-- MetadataOperand Metadata


doubleNum :: Double -> StateWithErr MetaData AST.Operand
doubleNum num = pure . AST.ConstantOperand . Const.Float $ Float.Double num












-- define ::  AST.Type -> AST.Name -> [(AST.Type, AST.Name)] -> [AST.BasicBlock]
--        -> StateWithErr MetaData AST.Definition
-- define retT funcName typedArgs body = do
--   St.modify $ \meta -> meta { definitionTable = Map.insert funcName definition
--                                                 $ definitionTable meta }
--   pure definition
--   where
--     definition = AST.GlobalDefinition
--       G.functionDefaults { G.name        = funcName
--                          , G.parameters  = (genParam <$> typedArgs, False)
--                          , G.returnType  = retT
--                          , G.basicBlocks = body }

-- external ::  AST.Type -> AST.Name -> [(AST.Type, AST.Name)]
--          -> StateWithErr MetaData AST.Definition
-- external retT funcName argtys = do
--   St.modify $ \meta -> meta { definitionTable = Map.insert funcName definition
--                                                 $ definitionTable meta }
--   pure definition
--   where
--     definition = AST.GlobalDefinition
--                  G.functionDefaults { G.name        = funcName
--                                     , G.parameters  = (genParam <$> argtys, False)
--                                     , G.returnType  = retT
--                                     , G.basicBlocks = [] }


-- -- Global

-- newGlobalFunc =


-- mangleName :: AST.Name -> Names -> (AST.Name, Names)
-- mangleName name names =
--   maybe
--   (name,  Map.insert name 1 names)
--   (\x -> (name <> nameShow x, Map.insert name (x + 1) names))
--   $ names !? name

-- newBlock :: AST.Name
--          -> [AST.Named AST.Instruction]
--          -> AST.Named AST.Terminator
--          -> StateWithErr MetaData AST.BasicBlock
-- newBlock blkName namedInstrs term = do
--   MetaData { names = names0 } <- St.get
--   let (mangledName, names1) = mangleName blkName names0
--   St.modify $ \s -> s { names = names1 }
--   pure $ AST.BasicBlock mangledName namedInstrs term

-- localRef :: AST.Name -> AST.Operand
-- localRef = AST.LocalReference Def.double

-- assign :: AST.Name -> AST.Operand -> StateWithErr MetaData ()
-- assign var operand =
--   St.modify $ \s -> s { symbolTable = Map.insert var operand $ symbolTable s }

-- getvar :: AST.Name -> StateWithErr MetaData AST.Operand
-- getvar var = do
--   MetaData { symbolTable = symbolTable } <- St.get
--   maybe (throwErr $ "Local variable not in scope: " <> tShow var) pure
--     $ symbolTable !? var

-- lessThan :: AST.Operand -> AST.Operand -> StateWithErr MetaData AST.Operand
-- lessThan a b = -- ui2fp double =<<
--   fCmp FP.ULT a b

-- fCmp :: FP.FloatingPointPredicate -> AST.Operand -> AST.Operand -> StateWithErr MetaData AST.Operand
-- fCmp cond a b = appendNamedInstr $ AST.FCmp cond a b []

-- ui2fp :: AST.Type -> AST.Operand -> StateWithErr MetaData AST.Operand
-- ui2fp type' a = appendNamedInstr $ AST.UIToFP a type' []

-- appendNamedInstr :: AST.Instruction -> StateWithErr MetaData AST.Operand
-- appendNamedInstr instr = do
--   MetaData { instrCount = instrCount } <- St.get
--   let refrence = AST.UnName $ instrCount + 1
--   St.modify $ \meta -> meta { namedInstrs = namedInstrs meta ++ [refrence := instr]
--                             , instrCount = instrCount + 1 }
--   pure $ localRef refrence

-- expr2module :: Expr -> StateWithErr MetaData AST.Definition
-- expr2module (FuncDef name args body) =
--   E.either throwErr
--   (\blocks -> define double name (toSig args) [blocks]) eitherBlocks
--   where
--     eitherBlocks = evalMetaData $ do
--       (\arg -> do
--           var <- alloca Def.double
--           store var (localRef arg)
--           assign arg var) `mapM_` args
--       genTerm <- expr2operand body
--       MetaData { namedInstrs = namedInstrs } <- St.get
--       newBlock name namedInstrs (termRet (Just genTerm))

-- expr2module (Extern name args) = external Def.double name (toSig args)

-- expr2module expr =
--   E.either throwErr
--   (\blocks -> define Def.double "main" [] [blocks]) eitherBlocks
--     where
--       eitherBlocks = evalMetaData $ do
--         genTerm <- expr2operand expr
--         MetaData { namedInstrs = namedInstrs } <- St.get
--         newBlock (genName "main") namedInstrs (termRet (Just genTerm))

-- toSig :: [AST.Name] -> [(AST.Type, AST.Name)]
-- toSig = map ((,) Def.double)

-- defsgenTop :: [Expr] -> StateWithErr MetaData [AST.Definition]
-- defsgenTop (expr:exprs) = do
--   def  <- expr2module expr
--   defs <- defsgenTop exprs
--   pure $ def:defs
-- defsgenTop [] = pure []


-- expr2operand :: Expr -> StateWithErr MetaData AST.Operand
-- expr2operand (Float float) = pure . AST.ConstantOperand . Const.Float . Float.Double $ float

-- expr2operand (Var x) = getvar x >>= load

-- expr2operand (BinOp op leftArg rightArg) =
--   May.maybe (throwErr $ "No such operator" <> tShow op)
--   (\operand -> M.join $ operand <$> expr2operand leftArg <*> expr2operand rightArg)
--   $ binops !? op

-- expr2operand (FuncCall fn args) = do
--   MetaData { definitionTable = definitionTable } <- St.get
--   May.maybe noFunc
--     (\definition -> do { operand <- externf definition
--                        ; args    <- mapM expr2operand args
--                        ; call operand args })
--     $ definitionTable !? fn
--     where
--       noFunc = throwErr $ "No such function: " <> tShow fn


-- externf :: AST.Definition -> StateWithErr MetaData AST.Operand
-- externf (AST.GlobalDefinition AST.Function {name = name, returnType = retT, parameters = parameters}) =
--   pure . AST.ConstantOperand . Const.GlobalReference
--   AST.PointerType { pointerAddrSpace = AddrSpace.AddrSpace 0
--                   , pointerReferent  = AST.FunctionType
--                                        { resultType    = retT
--                                        , argumentTypes = argTs
--                                        , isVarArg = False } }
--   $ name
--   where
--     argTs = (\(G.Parameter argT _ _) -> argT) <$> fst parameters
-- externf notFunc = throwErr $ tShow notFunc <> " is not function"

-- binops = Map.fromList [ (Plus, fAdd)
--                       , (Minus, fSub)
--                       , (Times, fMul)
--                       , (Divide, fDiv)
--                       , (LsT, lessThan)
--                       ]
