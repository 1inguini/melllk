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
import qualified LLVM.AST.Linkage                as Linkage
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

-- mangle :: AST.Name -> StateWithErr MetaData AST.Name
-- mangle name = do
--   MetaData { names = names } <- St.get
--   maybe
--     (do { St.modify $ \meta -> meta { names = Map.insert name 1 names }
--         ; pure name })
--     (\num -> do { St.modify $ \meta -> meta { names = Map.insert name (num + 1) names }
--                 ; pure $ name <> nameShow num })
--     $ names !? name

unName :: StateWithErr MetaData AST.Name
unName = do
  meta@MetaData { unusedNum = unusedNum } <- St.get
  St.put meta { unusedNum = unusedNum + 1 }
  pure $ AST.UnName unusedNum


ptrTo :: AST.Type -> AST.Type
ptrTo type' = AST.PointerType { pointerAddrSpace = AddrSpace.AddrSpace 0
                              , pointerReferent  = type' }


mayThrowErr :: T.Text -> Maybe a -> StateWithErr MetaData a
mayThrowErr _ (Just a)  = pure a
mayThrowErr msg Nothing = throwErr msg



-- Toplevels to Module

toplevels2module :: (AST.Module, MetaData) -> [Toplevel]
                 -> StateWithErr MetaData AST.Module
toplevels2module (module_, meta) exprs = do
  St.put meta
  maybeDefs <- toplevel2def `mapM` exprs
  pure module_ { AST.moduleDefinitions = mainlessDefs
                                         ++ fromMaybeList [] maybeDefs }
    where
      oldDefs      = AST.moduleDefinitions module_
      mainlessDefs = filter (not . isMainFunc) oldDefs

isMainFunc :: AST.Definition -> Bool
isMainFunc (AST.GlobalDefinition AST.Function { name = name }) = name == "main"
isMainFunc _ = False

fromMaybeList :: [a] -> [Maybe a] -> [a]
fromMaybeList accm (Just a:listMay)  = a:fromMaybeList accm listMay
fromMaybeList accm (Nothing:listMay) = fromMaybeList accm listMay
fromMaybeList accm []                = accm



-- Toplevel to Definition

toplevel2def :: Toplevel -> StateWithErr MetaData (Maybe AST.Definition)
toplevel2def top = do
  maybeGlobals <- toplevel2Global top
  pure $ AST.GlobalDefinition <$> maybeGlobals



-- TopLevel to Global

toplevel2Global :: Toplevel -> StateWithErr MetaData (Maybe AST.Global)
toplevel2Global (FuncDef name argNames bodyExpr) = do
  MetaData { definitionTable = definitionTable
           } <- St.get
  May.maybe funcDef (const . throwErr $ "duplicated definition: " <> tShow name)
    $ definitionTable !? name
  where
    funcDef = do
      -- name <- mangle name
      MetaData { symbolTable = oldSymbolTable
           -- , names       = oldNames
               } <- St.get
      -- St.modify $ \meta -> meta { names     = Map.empty
      --                           , unusedNum = 0 }
      argNInstrs <- concatMap namedInstrs <$> assign Type.double `mapM` argNames
      nios       <- expr2nio bodyExpr
      term       <- termRet $ mayOperand nios
        -- blkname    <- mangle (AST.Name "entry")
      body       <- newBasicBlock (AST.Name "entry") (argNInstrs ++ namedInstrs nios) term
      St.modify $ \meta -> meta { symbolTable = oldSymbolTable
                                -- , names       = oldNames
                                }
      defineFunc double name ((,) double <$> argNames) [body]


toplevel2Global (Extern name argNames) =
  externFunc double name ((,) double <$> argNames)

toplevel2Global (Expr expr) = do
  MetaData { symbolTable = oldSymbolTable
           -- , names       = oldNames
           } <- St.get
  nios       <- expr2nio expr
  term       <- termRet $ mayOperand nios
  -- blkname    <- mangle (AST.Name "entry")
  body       <- newBasicBlock (AST.Name "entry") (namedInstrs nios) term
  St.modify $ \meta -> meta { symbolTable = oldSymbolTable
                            -- , names       = oldNames
                            }
  defineFunc double (AST.Name "main") [] [body]



-- Exprs to Basicblock

-- exprs2BBlock :: [Expr] -> StateWithErr MetaData AST.BasicBlock
-- exprs2BBlock exprs = do
--   nios    <- expr2nio `mapM` exprs
--   term    <- termRet $ Safe.lastDef Nothing (mayOperand <$> nios)
--   blkname <- mangle (AST.Name "entry")
--   newBasicBlock blkname (concat $ namedInstrs <$> nios) term



-- Expr to  Named Instructions and Operand

expr2nio :: Expr -> StateWithErr MetaData NInstrsOperand
expr2nio (Float num)   = do
  numOp <- doubleNum num
  pure defNIO { mayOperand = Just numOp }

expr2nio (Var varName) = do
  MetaData { symbolTable = symbolTable } <- St.get
  mayThrowErr ("variable not assigned: " <> tShow varName)
    (symbolTable !? varName)
    >>= load

expr2nio (BinOp op expr0 expr1) = do
  NIO { mayOperand  = mayOperand0
      , namedInstrs = namedInstrs0 } <- expr2nio expr0
  NIO { mayOperand  = mayOperand1
      , namedInstrs = namedInstrs1 } <- expr2nio expr1
  operand0 <- mayThrowErr ("failed at: " <> tShow expr0) mayOperand0
  operand1 <- mayThrowErr ("failed at: " <> tShow expr0) mayOperand1
  NIO { mayOperand  = resultOperand
      , namedInstrs = resultNInstrs
      } <- (case op of
               ; Plus   -> fAdd
               ; Minus  -> fSub
               ; Times  -> fMul
               ; Divide -> fDiv
               ; LsT    -> undefined) operand0 operand1
  pure defNIO { mayOperand  = resultOperand
              , namedInstrs = namedInstrs0
                              ++ namedInstrs1
                              ++ resultNInstrs }

expr2nio (FuncCall name exprs) = do
  nios        <- expr2nio `mapM` exprs
  argOps      <- mayThrowErr ("failed at:" <> tShow exprs) `mapM` (mayOperand <$> nios)
  MetaData { definitionTable = definitionTable } <- St.get
  NIO { mayOperand  = retMayOp
      , namedInstrs = retNInstrs
      } <- May.maybe
           (throwErr $ "function not defined: " <> tShow name)
           (flip (call Type.double) argOps)
           $ definitionTable !? name
  pure defNIO { mayOperand  = retMayOp
              , namedInstrs = concat (namedInstrs <$> nios)
                              ++ retNInstrs }



-- Global

defineFunc :: AST.Type -> AST.Name -> [(AST.Type, AST.Name)] -> [AST.BasicBlock]
           -> StateWithErr MetaData (Maybe AST.Global)
defineFunc type' name typedArgs basicBlocks = do
  addToDefinitionTable type' name typedArgs
  pure . Just $
    AST.functionDefaults { G.returnType  = type'
                         , G.name        = name
                         , G.parameters  = (genParam <$> typedArgs, False)
                         , G.basicBlocks = basicBlocks }


externFunc :: AST.Type -> AST.Name -> [(AST.Type, AST.Name)]
           -> StateWithErr MetaData (Maybe AST.Global)
externFunc type' name typedArgs = do
  addToDefinitionTable type' name typedArgs
  pure . Just $
    AST.functionDefaults
    { G.linkage    = Linkage.External
    , G.returnType = type'
    , G.name       = name
    , G.parameters = (genParam <$> typedArgs, False)
    }


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

-- Misc (Global)

genParam :: (AST.Type, AST.Name) -> AST.Parameter
genParam (type', name) = G.Parameter type' name []



-- Basicblock

newBasicBlock :: AST.Name -> [AST.Named AST.Instruction] -> AST.Named AST.Terminator
           -> StateWithErr MetaData AST.BasicBlock
newBasicBlock blkname namedInstrs terminator =
  pure $ AST.BasicBlock blkname namedInstrs terminator



-- Named Instruction and Operand (NIO)

fAdd, fSub, fMul, fDiv :: AST.Operand -> AST.Operand
  -> StateWithErr MetaData NInstrsOperand
fAdd = fOps AST.FAdd
fSub = fOps AST.FSub
fMul = fOps AST.FMul
fDiv = fOps AST.FDiv


call :: AST.Type -> AST.Operand -> [AST.Operand] -> StateWithErr MetaData NInstrsOperand
call type' fn args = do
  name <- unName
  pure defNIO { mayOperand = Just $ AST.LocalReference type' name
              , namedInstrs =
                [name := AST.Call Nothing CallC.C [] (Right fn) ((\x -> (x, [])) <$> args) [] []] }

alloca :: AST.Type -> StateWithErr MetaData NInstrsOperand
alloca type' = do
  name <- unName
  pure defNIO { mayOperand  = Just $ AST.LocalReference (ptrTo type') name
              , namedInstrs = [name := AST.Alloca type' Nothing 0 []] }


store :: AST.Operand -> AST.Operand -> StateWithErr MetaData NInstrsOperand
store addr val =
  pure defNIO { namedInstrs = [AST.Do $ AST.Store False addr val Nothing 0 []] }


load :: AST.Operand -> StateWithErr MetaData NInstrsOperand
load addr@(AST.LocalReference AST.PointerType { pointerReferent = type' } _) = do
  name <- unName
  pure defNIO { mayOperand = Just $ AST.LocalReference type' name
              , namedInstrs = [name := AST.Load False addr Nothing 0 []]}
load operand = throwErr ("Cannot load non-pointer (Malformed AST): " <> tShow operand)


assign :: AST.Type -> AST.Name -> StateWithErr MetaData NInstrsOperand
assign type' name = do
  NIO { namedInstrs = allocaNInstrs
      , mayOperand  = mayAllocaOp
      }  <- alloca type'
  addrOp <- mayThrowErr ("failed at assigning (alloca): " <> tShow name) mayAllocaOp
  NIO { namedInstrs = storeNInstrs
      }  <- store addrOp (AST.LocalReference type' name)
  St.modify $ \meta -> meta { symbolTable = Map.insert name addrOp $ symbolTable meta }
  pure defNIO { mayOperand  = mayAllocaOp
              , namedInstrs = allocaNInstrs ++ storeNInstrs }


-- Misc (Named Instruction)

fOps :: (AST.FastMathFlags -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction)
     -> AST.Operand -> AST.Operand -> StateWithErr MetaData NInstrsOperand
fOps constr a b = do
  name <- unName
  pure defNIO { mayOperand  = Just $ AST.LocalReference double name
              , namedInstrs = [name := constr AST.noFastMathFlags a b []] }



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
