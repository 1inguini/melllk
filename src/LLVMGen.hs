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
import qualified LLVM.Prelude                    as LP

genLLVM :: AST.Module -> IO BStr.ByteString
genLLVM module_ = Context.withContext $ \context ->
  LLVM.withModuleFromAST context module_ LLVM.moduleLLVMAssembly

namedDefModule :: T.Text -> AST.Module
namedDefModule label = AST.defaultModule { AST.moduleName = fromText label }

appendDefn :: AST.Definition -> StateWithErr [AST.Definition] [AST.Definition]
appendDefn def = St.modify (++ [def]) >> St.get

define ::  AST.Type -> AST.Name -> [(AST.Type, AST.Name)] -> [AST.BasicBlock]
       -> StateWithErr MetaData AST.Definition
define retT funcName typedArgs body = do
  St.modify $ \meta -> meta { definitionTable = Map.insert funcName definition
                                                $ definitionTable meta }
  pure definition
  where
    definition = AST.GlobalDefinition
      G.functionDefaults { G.name        = funcName
                         , G.parameters  = (genParam <$> typedArgs, False)
                         , G.returnType  = retT
                         , G.basicBlocks = body }

external ::  AST.Type -> AST.Name -> [(AST.Type, AST.Name)]
         -> StateWithErr MetaData AST.Definition
external retT funcName argtys = do
  St.modify $ \meta -> meta { definitionTable = Map.insert funcName definition
                                                $ definitionTable meta }
  pure definition
  where
    definition = AST.GlobalDefinition
                 G.functionDefaults { G.name        = funcName
                                    , G.parameters  = (genParam <$> argtys, False)
                                    , G.returnType  = retT
                                    , G.basicBlocks = [] }

genParam :: (AST.Type, AST.Name) -> AST.Parameter
genParam (type_, name) = G.Parameter type_ name []

mangleName :: AST.Name -> Names -> (AST.Name, Names)
mangleName name names =
  maybe
  (name,  Map.insert name 1 names)
  (\x -> (name <> nameShow x, Map.insert name (x + 1) names))
  $ names !? name

newBlock :: AST.Name
         -> [AST.Named AST.Instruction]
         -> AST.Named AST.Terminator
         -> StateWithErr MetaData AST.BasicBlock
newBlock blkName namedInstrs term = do
  MetaData { names = names0 } <- St.get
  let (mangledName, names1) = mangleName blkName names0
  St.modify $ \s -> s { names = names1 }
  pure $ AST.BasicBlock mangledName namedInstrs term

localRef :: AST.Name -> AST.Operand
localRef = AST.LocalReference Def.double

assign :: AST.Name -> AST.Operand -> StateWithErr MetaData ()
assign var operand =
  St.modify $ \s -> s { symbolTable = Map.insert var operand $ symbolTable s }

getvar :: AST.Name -> StateWithErr MetaData AST.Operand
getvar var = do
  MetaData { symbolTable = symbolTable } <- St.get
  maybe (throwErr $ "Local variable not in scope: " <> tShow var) pure
    $ symbolTable !? var

searchBBlockName :: AST.Name -> [AST.BasicBlock] -> Maybe AST.BasicBlock
searchBBlockName name = Ls.find ((name ==) . basicBlockName)

fAdd, fSub, fMul, fDiv :: AST.Operand -> AST.Operand -> StateWithErr MetaData AST.Operand
fAdd = fOps AST.FAdd
fSub = fOps AST.FSub
fMul = fOps AST.FMul
fDiv = fOps AST.FDiv

fOps :: (AST.FastMathFlags -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction)
     -> AST.Operand -> AST.Operand -> StateWithErr MetaData AST.Operand
fOps constr a b = appendNamedInstr $ constr AST.noFastMathFlags a b []

lessThan :: AST.Operand -> AST.Operand -> StateWithErr MetaData AST.Operand
lessThan a b = -- ui2fp double =<<
  fCmp FP.ULT a b

fCmp :: FP.FloatingPointPredicate -> AST.Operand -> AST.Operand -> StateWithErr MetaData AST.Operand
fCmp cond a b = appendNamedInstr $ AST.FCmp cond a b []

ui2fp :: AST.Type -> AST.Operand -> StateWithErr MetaData AST.Operand
ui2fp type_ a = appendNamedInstr $ AST.UIToFP a type_ []

appendNamedInstr :: AST.Instruction -> StateWithErr MetaData AST.Operand
appendNamedInstr instr = do
  MetaData { instrCount = instrCount } <- St.get
  let refrence = AST.UnName $ instrCount + 1
  St.modify $ \meta -> meta { namedInstrs = namedInstrs meta ++ [refrence := instr]
                            , instrCount = instrCount + 1 }
  pure $ localRef refrence

call :: AST.Operand -> [AST.Operand] -> StateWithErr MetaData AST.Operand
call fn args = appendNamedInstr $
  AST.Call Nothing CallC.C [] (Right fn) ((\x -> (x, [])) <$> args) [] []

alloca :: AST.Type -> StateWithErr MetaData AST.Operand
alloca type_ = appendNamedInstr $ AST.Alloca type_ Nothing 0 []

store :: AST.Operand -> AST.Operand -> StateWithErr MetaData ()
store addr val = appendDoInstr $ AST.Store False addr val Nothing 0 []

appendDoInstr :: AST.Instruction -> StateWithErr MetaData ()
appendDoInstr instr = do
  St.modify $ \meta -> meta { namedInstrs = namedInstrs meta ++ [AST.Do instr] }

load :: AST.Operand -> StateWithErr MetaData AST.Operand
load addr = appendNamedInstr $ AST.Load False addr Nothing 0 []

termRet :: Maybe AST.Operand -> Named AST.Terminator
termRet retOperand = AST.Do $ AST.Ret retOperand []

termBr :: AST.Name -> Named AST.Terminator
termBr dest = AST.Do $ AST.Br dest []

termCondbr :: AST.Operand -> AST.Name -> AST.Name -> Named AST.Terminator
termCondbr cond tr fl = AST.Do $ AST.CondBr cond tr fl []

expr2module :: Expr -> StateWithErr MetaData AST.Definition
expr2module (FuncDef name args body) =
  E.either throwErr
  (\blocks -> define double name (toSig args) [blocks]) eitherBlocks
  where
    eitherBlocks = evalMetaData $ do
      mapM_ (\arg -> do
                var <- alloca Def.double
                store var (localRef arg)
                assign arg var) args
      genTerm <- cgen body
      MetaData { namedInstrs = namedInstrs } <- St.get
      newBlock name namedInstrs (termRet (Just genTerm))

expr2module (Extern name args) = external Def.double name (toSig args)

expr2module expr =
  E.either throwErr
  (\blocks -> define Def.double "main" [] [blocks]) eitherBlocks
    where
      eitherBlocks = evalMetaData $ do
        genTerm <- cgen expr
        MetaData { namedInstrs = namedInstrs } <- St.get
        newBlock (genName "main") namedInstrs (termRet (Just genTerm))

toSig :: [AST.Name] -> [(AST.Type, AST.Name)]
toSig = map ((,) Def.double)

defsgenTop :: [Expr] -> StateWithErr MetaData [AST.Definition]
defsgenTop (expr:exprs) = do
  def  <- expr2module expr
  defs <- defsgenTop exprs
  pure $ def:defs
defsgenTop [] = pure []

codegenTop :: [Expr] -> StateWithErr MetaData AST.Module
codegenTop exprs = do
  defs <- defsgenTop exprs
  pure $ AST.defaultModule { AST.moduleDefinitions = defs }

cgen :: Expr -> StateWithErr MetaData AST.Operand
cgen (Float float) = pure . AST.ConstantOperand . Const.Float . Float.Double $ float

cgen (Var x) = getvar x >>= load

cgen (BinOp op leftArg rightArg) =
  May.maybe (throwErr $ "No such operator" <> tShow op)
  (\operand -> M.join $ operand <$> cgen leftArg <*> cgen rightArg)
  $ binops !? op

cgen (FuncCall fn args) = do
  MetaData { definitionTable = definitionTable } <- St.get
  May.maybe noFunc
    (May.maybe noFunc
     (\operand -> mapM cgen args >>= call operand)
      . externf)
    $ definitionTable !? fn
    where
      noFunc = throwErr $ "No such function" <> tShow fn

externf :: AST.Definition -> Maybe AST.Operand
externf (AST.GlobalDefinition AST.Function { name       = name
                                           , returnType = retT
                                           , parameters = parameters }) =
  pure . AST.ConstantOperand . Const.GlobalReference
  AST.PointerType { pointerAddrSpace = AddrSpace.AddrSpace 0
                  , pointerReferent  = AST.FunctionType
                                       { resultType    = retT
                                       , argumentTypes = argTs
                                       , isVarArg = False } }
  $ name
  where
    argTs = (\(G.Parameter argT _ _) -> argT) <$> fst parameters
externf _ = Nothing

binops = Map.fromList [ (Plus, fAdd)
                      , (Minus, fSub)
                      , (Times, fMul)
                      , (Divide, fDiv)
                      , (LsT, lessThan)
                      ]
