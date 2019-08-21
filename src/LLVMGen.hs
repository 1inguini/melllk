{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module LLVMGen where

import           Definition                 as Def

import           Prelude                    as P

import qualified Control.Monad.State        as St
import qualified Data.ByteString            as BStr
import qualified Data.Either                as E
import qualified Data.Function              as Func
import qualified Data.List                  as Ls
import           Data.Map                   ((!?))
import qualified Data.Map                   as Map
import qualified Data.Text                  as T

import qualified LLVM
import           LLVM.AST                   (Named ((:=)))
import qualified LLVM.AST                   as AST
import qualified LLVM.AST.CallingConvention as CallC
import qualified LLVM.AST.Constant          as Const
import qualified LLVM.AST.Float             as Float
import qualified LLVM.AST.Global            as G
import qualified LLVM.AST.IntegerPredicate  as IP
import qualified LLVM.AST.Type              as Type
import qualified LLVM.Context               as Context
import qualified LLVM.Prelude               as LP

genLLVM :: AST.Module -> IO BStr.ByteString
genLLVM module_ = Context.withContext $ \ctx ->
  LLVM.withModuleFromAST ctx module_ LLVM.moduleLLVMAssembly

namedDefModule :: T.Text -> AST.Module
namedDefModule label = AST.defaultModule { AST.moduleName = fromText label }

appendDefn :: AST.Definition -> St.State AST.Module ()
appendDefn def = St.modify $ \s ->
  s { AST.moduleDefinitions = AST.moduleDefinitions s ++ [def] }

define ::  AST.Type -> AST.Name -> [(AST.Type, AST.Name)] -> [AST.BasicBlock]
       -> St.State AST.Module ()
define retT funcName typedArgs body =
  appendDefn $ AST.GlobalDefinition
    G.functionDefaults { G.name        = funcName
                       , G.parameters  = (genParam <$> typedArgs, False)
                       , G.returnType  = retT
                       , G.basicBlocks = body
                       }

external ::  AST.Type -> AST.Name -> [(AST.Type, AST.Name)]
         -> St.State AST.Module ()
external retT funcName argtys =
  appendDefn $ AST.GlobalDefinition
  G.functionDefaults { G.name        = funcName
                     , G.parameters  = (genParam <$> argtys, False)
                     , G.returnType  = retT
                     , G.basicBlocks = []
                     }

genParam :: (AST.Type, AST.Name) -> AST.Parameter
genParam (ty, nm) = G.Parameter ty nm []

mangleName :: AST.Name -> Names -> (AST.Name, Names)
mangleName name names =
  maybe
  (name,  Map.insert name 1 names)
  (\x -> (name <> nameShow x, Map.insert name (x + 1) names))
  $ names !? name

newBlock :: AST.Name
         -> [AST.Named AST.Instruction]
         -> AST.Named AST.Terminator
         -> St.State Code AST.BasicBlock
newBlock blkName namedInstrs term = do
  Code { names = names0 } <- St.get
  let (mangledName, names1) = mangleName blkName names0
  St.modify $ \s -> s { names = names1 }
  pure $ AST.BasicBlock mangledName namedInstrs term

localVar :: AST.Name -> AST.Operand
localVar = AST.LocalReference Def.double

externf :: AST.Name -> AST.Operand
externf = AST.ConstantOperand . Const.GlobalReference Def.double

assign :: AST.Name -> AST.Operand -> St.State Code ()
assign var operand =
  St.modify $ \s -> s { symboltable = Map.insert var operand $ symboltable s }

getvar :: AST.Name -> St.State Code (Either T.Text AST.Operand)
getvar var = do
  Code { symboltable = symboltable } <- St.get
  maybe (pure . Left $ "Local variable not in scope: " <> tShow var) (pure . Right)
    $ symboltable !? var

searchBBlockName :: AST.Name -> [AST.BasicBlock] -> Maybe AST.BasicBlock
searchBBlockName name = Ls.find ((name ==) . basicBlockName)

-- terminator :: AST.Name -> Named AST.Terminator -> St.State Code (Maybe (Named AST.Terminator))
-- terminator blkName term = do
--   Code { blocks     = blocks } <- St.get
--   maybe
--     (pure Nothing)
--     (\block -> do
--         modifyBlock blkName $ \b -> b { term = pure term }
--         pure $ Just term)
--     $ blocks !? blkName

-- modifyBlock :: AST.Name -> (Block -> Block) -> St.State Code ()
-- modifyBlock blkName block = do
--   Code { blocks     = blocks } <- St.get
--   maybe
--     (St.modify id)
--     (\b ->
--        St.modify $ \s ->
--         s {blocks = Map.insert blkName (block b) blocks})
--     $ blocks !? blkName

fadd, fsub, fmul, fdiv :: AST.Operand -> AST.Operand -> St.State Code AST.Operand
fadd = fOps AST.FAdd
fsub = fOps AST.FSub
fmul = fOps AST.FMul
fdiv = fOps AST.FDiv

fOps :: (AST.FastMathFlags -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction)
     -> AST.Operand -> AST.Operand -> St.State Code AST.Operand
fOps constr a b = appendNamedInstr $ constr AST.noFastMathFlags a b []

appendNamedInstr :: AST.Instruction -> St.State Code AST.Operand
appendNamedInstr instr = do
  Code { instrCount = instrCount } <- St.get
  let refrence = AST.UnName $ instrCount + 1
  St.modify $ \code -> code { namedInstrs = namedInstrs code ++ [refrence := instr]
                            , instrCount = instrCount + 1 }
  pure $ localVar refrence

call :: AST.Operand -> [AST.Operand] -> St.State Code AST.Operand
call fn args = appendNamedInstr $
  AST.Call Nothing CallC.C [] (Right fn) ((\x -> (x, [])) <$> args) [] []

alloca :: AST.Type -> St.State Code AST.Operand
alloca ty = appendNamedInstr $ AST.Alloca ty Nothing 0 []

store :: AST.Operand -> AST.Operand -> St.State Code AST.Operand
store ptr val = appendNamedInstr $ AST.Store False ptr val Nothing 0 []

load :: AST.Operand -> St.State Code AST.Operand
load ptr = appendNamedInstr $ AST.Load False ptr Nothing 0 []

termRet :: Maybe AST.Operand -> Named AST.Terminator
termRet mayVal = AST.Do $ AST.Ret mayVal []

termBr :: AST.Name -> Named AST.Terminator
termBr val = AST.Do $ AST.Br val []

termCondbr :: AST.Operand -> AST.Name -> AST.Name -> Named AST.Terminator
termCondbr cond tr fl = AST.Do $ AST.CondBr cond tr fl []

codegenTop :: Expr -> St.State AST.Module ()
codegenTop (FuncDef name args body) =
  define double name fnargs [blocks]
  where
    fnargs = toSig args
    blocks = evalCode $ do
      mapM_ (\a -> do
                var <- alloca Def.double
                store var (localVar a)
                assign a var) args
      genTerm <- cgen body
      Code {namedInstrs = namedInstrs} <- St.get
      newBlock name namedInstrs (termRet (Just genTerm))

codegenTop (Extern name args) = do
  external Def.double name fnargs
  where fnargs = toSig args

codegenTop expr =
  define Def.double "main" [] [blocks]
    where
      blocks = evalCode $ do
        genTerm <- cgen expr
        Code {namedInstrs = namedInstrs} <- St.get
        newBlock (genName "main") namedInstrs (termRet (Just genTerm))

execCode :: St.State Code a -> Code
execCode m = St.execState m emptyCode

evalCode :: St.State Code a -> a
evalCode m = St.evalState m emptyCode

runCode :: St.State Code a -> (a, Code)
runCode m = St.runState m emptyCode

-- sortBlocks :: [(AST.Name, Block)] -> [(AST.Name, Block)]
-- sortBlocks = Ls.sortBy (compare `Func.on` (Def.index . snd))

-- createBlocks :: Code -> Either T.Text [AST.BasicBlock]
-- createBlocks m = (\eitherBB ->
--                     if any E.isLeft eitherBB
--                     then Right $ E.rights eitherBB
--                     else Left $ foldl (\x y -> x <> "\n" <> y) "" (E.lefts eitherBB))
--                  $ makeBlock <$> sortBlocks (Map.toList $ blocks m)
--   where
--     makeBlock :: (AST.Name, Block) -> Either T.Text AST.BasicBlock
--     makeBlock (l, Block _ s t) = AST.BasicBlock l (reverse s) <$> maketerm t
--       where
--         maketerm (Just x) = Right x
--         maketerm Nothing  = Left $ "Block has no terminator: " <> tShow l

toSig :: [AST.Name] -> [(AST.Type, AST.Name)]
toSig = map ((,) Def.double)

cgen :: Expr -> St.State Code AST.Operand
cgen (Float float) =
  pure $ AST.ConstantOperand . Const.Float . Float.Double $ float
  -- St.modify $ \code -> code { blockTerm = termRet term }

cgen (BinOp op a b) =
  case Map.lookup op binops of
    Just func -> do
      ca <- cgen a
      cb <- cgen b
      func ca cb
    Nothing -> error $ "No such operator" ++ show op

binops = Map.fromList [ (Plus, fadd)
                      , (Minus, fsub)
                      , (Times, fmul)
                      , (Divide, fdiv)
                      ]
