{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module LLVMGen where

import           Definition                as Def

import           Prelude                   as P

import qualified Control.Monad.State       as St
import qualified Data.ByteString           as BStr
import qualified Data.Function             as Func
import qualified Data.List                 as Ls
import           Data.Map                  ((!?))
import qualified Data.Map                  as Map
import qualified Data.Text                 as T

import qualified LLVM
import           LLVM.AST                  (Named ((:=)))
import qualified LLVM.AST                  as AST
import qualified LLVM.AST.Constant         as Const
import qualified LLVM.AST.Float            as Float
import qualified LLVM.AST.Global           as G
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type             as Type
import qualified LLVM.Context              as Context
import qualified LLVM.Prelude              as LP

genLLVM :: AST.Module -> IO BStr.ByteString
genLLVM mod = Context.withContext $ \ctx ->
  LLVM.withModuleFromAST ctx mod LLVM.moduleLLVMAssembly

emptyModule :: T.Text -> AST.Module
emptyModule label = AST.defaultModule { AST.moduleName = fromText label }

appendDefn :: AST.Definition -> St.State AST.Module ()
appendDefn def = St.modify $ \s ->
  s { AST.moduleDefinitions = AST.moduleDefinitions s ++ [def] }

define ::  AST.Type -> T.Text -> [(AST.Type, AST.Name)] -> [AST.BasicBlock]
       -> St.State AST.Module ()
define retT funcName argtys body =
  appendDefn $ AST.GlobalDefinition
  G.functionDefaults { G.name        = genName funcName
                     , G.parameters  = (genParam <$> argtys, False)
                     , G.returnType  = retT
                     , G.basicBlocks = body
                     }

external ::  AST.Type -> T.Text -> [(AST.Type, AST.Name)]
         -> St.State AST.Module ()
external retT funcName argtys =
  appendDefn $ AST.GlobalDefinition
  G.functionDefaults { G.name        =  genName funcName
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

genBlock :: AST.Name -> Block -> St.State Code ()
genBlock blkName block = do
  Code { blocks     = blocks
       , blockCount = blockCount
       , names      = names0 } <- St.get
  let (mangledName, names1) = mangleName blkName names0
      newBlock              = block { Def.index = blockCount }
  St.modify $ \s ->
    s { blocks     = Map.insert mangledName newBlock blocks
      , names      = names1
      , blockCount = blockCount + 1 }

-- setBlock :: Name -> St.State Code AST.Name
-- setBlock bname = do
--   modify $ \s -> s { currentBlock = bname }
--   return bname

-- getBlock :: St.State Code AST.Name
-- getBlock = gets currentBlock

-- current :: St.State Code Block
-- current = do
--   Code { currentBlock = currentBlock
--                , blocks       = blocks } <- get
--   maybe (error $ "No such block: " ++ show currentBlock) pure
--     $ blocks !? currentBlock

fresh :: St.State Code ()
fresh = St.modify $ \s -> s { instrCount = instrCount s + 1 }

local :: AST.Name -> AST.Operand
local = AST.LocalReference Def.double

externf :: AST.Name -> AST.Operand
externf = AST.ConstantOperand . Const.GlobalReference Def.double

assign :: T.Text -> AST.Operand -> St.State Code ()
assign var operand = do
  Code { symboltable = symboltable } <- St.get
  St.modify $ \s -> s { symboltable = Map.insert var operand symboltable }

getvar :: T.Text -> St.State Code AST.Operand
getvar var = do
  Code { symboltable = symboltable } <- St.get
  maybe (error $ "Local variable not in scope: " ++ show var) pure
    $ symboltable !? var

instr :: AST.Instruction -> St.State Code AST.Operand
instr ins = do
  ref <- AST.UnName <$> St.gets instrCount
  genBlock ref
    emptyBlock { stack = ref := ins
               : stack emptyBlock }
  pure $ local ref

-- terminator :: Named Terminator -> St.State Code (Named Terminator)
-- terminator term = do
--   blk <- current
--   modifyBlock blk { term = pure term }
--   pure term

-- codegenTop :: Expr -> St.State AST.Module ()
-- codegenTop (FuncDef name args body) =
--   define Def.double name fnargs bls
--   where
--     fnargs = toSig args
--     bls = createBlocks $ execCode $ do
--       entry <- addBlock entryBlockName
--       setBlock entry
--       (\a -> do
--           var <- alloca Def.double Nothing 0
--           store var 0 (local (genName a))
--           assign a var) <$> args
--       cgen body >>= ret

-- codegenTop (Extern name args) = do
--   external Def.double name fnargs
--   where fnargs = toSig args

-- codegenTop exp = do
--   define Def.double "main" [] blks
--   where
--     blks = createBlocks $ execCode $ do
--       entry <- addBlock entryBlockName
--       setBlock entry
--       x <- cgen exp
--       ret x

execCode :: St.State Code a -> Code
execCode m = St.execState m emptyCode

sortBlocks :: [(AST.Name, Block)] -> [(AST.Name, Block)]
sortBlocks = Ls.sortBy (compare `Func.on` (Def.index . snd))

createBlocks :: Code -> [AST.BasicBlock]
createBlocks m = makeBlock <$> sortBlocks (Map.toList $ blocks m)
  where
    makeBlock :: (AST.Name, Block) -> AST.BasicBlock
    makeBlock (l, Block _ s t) = AST.BasicBlock l (reverse s) (maketerm t)
      where
        maketerm (Just x) = x
        maketerm Nothing  = error $ "Block has no terminator: " ++ show l


toSig :: [T.Text] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (Def.double, genName x))

cgen :: Expr -> St.State Code AST.Operand
cgen (Float n) = pure $
  AST.ConstantOperand . Const.Float . Float.Double $ n
