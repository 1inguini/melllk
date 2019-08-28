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

genLLVM :: AST.Module -> IO BStr.ByteString
genLLVM module' = Context.withContext $ \context ->
  LLVM.withModuleFromAST context module' LLVM.moduleLLVMAssembly

-- initialize type' valOp = do
--   unName <- unName
--   var <- alloca type' unName
--   store var 0 valOp


zero :: AST.Operand
zero = AST.ConstantOperand .  Const.Float . Float.Double $ 0

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


ptrTo :: AST.Type -> AST.Type
ptrTo type' = AST.PointerType { pointerAddrSpace = AddrSpace.AddrSpace 0
                              , pointerReferent  = type' }



-- Toplevels to Module

toplevels2module :: (AST.Module, MetaData) -> [Toplevel]
                 -> StateWithErr MetaData AST.Module
toplevels2module (module_, meta) tops = do
  St.put meta
  maybeDefs <- toplevel2def `mapM` tops
  pure module_ { AST.moduleDefinitions = mainlessDefs
                                         ++ May.catMaybes maybeDefs }
    where
      oldDefs      = AST.moduleDefinitions module_
      mainlessDefs = filter (not . isMainFunc) oldDefs

isMainFunc :: AST.Definition -> Bool
isMainFunc (AST.GlobalDefinition AST.Function { name = name }) = name == "main"
isMainFunc _ = False



-- Toplevel to Definition

toplevel2def :: Toplevel -> StateWithErr MetaData (Maybe AST.Definition)
toplevel2def top = do
  maybeGlobals <- toplevel2Global top
  pure $ AST.GlobalDefinition <$> maybeGlobals



-- TopLevel to Global

toplevel2Global :: Toplevel -> StateWithErr MetaData (Maybe AST.Global)
toplevel2Global (FuncDef name argNames bodyExpr) = do
  MetaData { symbolTable = oldSymbolTable
           } <- St.get
  argNameds  <- concat . (nameds <$>) <$> assign Type.double `mapM` argNames
  OpNameds { mayOperand = mayOperand
           , nameds     = nameds
           } <- expr2opNm bodyExpr
  St.modify $ \meta -> meta { symbolTable = oldSymbolTable }
  let body = bblkConts2BBlock $
             argNameds
             ++ nameds
             ++ [NTerm $ termRet mayOperand]
  pure $ defineFunc double name typedArgs body
  where
    typedArgs = (,) double <$> argNames

toplevel2Global (Extern name argNames) = pure $ externFunc double name typedArgs
    where
      typedArgs = (,) double <$> argNames

toplevel2Global (Expr expr) = do
  MetaData { symbolTable = oldSymbolTable
           , names       = oldNames
           } <- St.get
  OpNameds { mayOperand = mayOperand
           , nameds     = nameds
           } <- expr2opNm expr
  let body = bblkConts2BBlock $ nameds ++ [NTerm $ termRet mayOperand]
  St.modify $ \meta -> meta { symbolTable = oldSymbolTable
                            , names       = oldNames
                            }
  pure $ defineFunc double (AST.Name "main") [] body

-- Operand and Basicblocks to Basicblock

bblkConts2BBlock :: BBlkContents -> [AST.BasicBlock]
bblkConts2BBlock = internal (AST.Name "entry") [] []
  where
    internal _ accmBlks accmInstrs (BlkName blkName:nameds) =
      internal blkName accmBlks accmInstrs nameds
    internal blkName accmBlks accmInstrs (NInstr namedInstr:nameds) =
      internal blkName accmBlks (accmInstrs ++ [namedInstr]) nameds
    internal blkName accmBlks accmInstrs (NTerm namedTerm:nameds) =
      internal blkName (accmBlks ++ [AST.BasicBlock blkName accmInstrs namedTerm]) [] nameds
    internal _ accmBlks [] [] =
      accmBlks
    internal blkName accmBlks accmInstrs [] =
      accmBlks ++ [AST.BasicBlock blkName accmInstrs (termRet Nothing)]


  -- term <- termRet $ Safe.lastDef Nothing (mayOperand <$> opNms)
  -- name <- mangle name
  -- bblk <- AST.BasicBlock name (concat $ namedInstrs <$> opNms) term
  -- pure $ concat (basicBlocks <$> opNms) ++ [bblk]


-- Exprs to Basicblock

-- expr2BBlock (If condExpr thenExpr elseExpr) = undefined
-- expr2BBlock expr = do
--   opNms     <- expr2opNm expr
--   body       <- opNms2BBlock (AST.Name "entry") opNms
--   pure body






-- Exprs to Named Instructions and Operand (OpBBlockRems)

-- exprs2opNms :: [Expr] -> StateWithErr MetaData [OpBBlockRem]
-- exprs2opNms (expr:exprs) = (:) <$> expr2opNm expr <*> exprs2opNms exprs
-- exprs2opNms []           = pure []

-- Expr to Named Instructions and Operand (OpBBlockRem)

expr2opNm :: Expr -> StateWithErr MetaData OpNameds
expr2opNm (Float num)   = do
  numOp <- doubleNum num
  pure defOpNs { mayOperand = Just numOp }

expr2opNm (Var varName) = do
  MetaData { symbolTable = symbolTable } <- St.get
  mayThrowErr ("variable not assigned: " <> tShow varName)
    (symbolTable !? varName)
    >>= load

expr2opNm (BinOp op expr0 expr1) = do
  OpNameds { mayOperand  = mayOperand0
           , nameds      = nameds0
           } <- expr2opNm expr0
  OpNameds { mayOperand  = mayOperand1
           , nameds = nameds1
           } <- expr2opNm expr1
  operand0   <- mayThrowErr ("failed at: " <> tShow expr0) mayOperand0
  operand1   <- mayThrowErr ("failed at: " <> tShow expr0) mayOperand1
  OpNameds { mayOperand  = resultOperand
           , nameds = resultNInstrs
           } <- (case op of
                   ; Plus   -> fAdd
                   ; Minus  -> fSub
                   ; Times  -> fMul
                   ; Divide -> fDiv
                   ; LsT    -> fCmp FP.OLT) operand0 operand1
  pure defOpNs { mayOperand  = resultOperand
               , nameds = nameds0 ++
                          nameds1 ++
                          resultNInstrs }

expr2opNm (FuncCall name args) = do
  opNms <- expr2opNm `mapM` args
  argOps <- mayThrowErr ("failed at: " <> tShow args) `mapM` (mayOperand <$> opNms)
  OpNameds
    { mayOperand  = retMayOp
    , nameds      = retNameds
    } <- call Type.double (genFunTypePtr double name (double <$ argOps)) argOps
  pure defOpNs { mayOperand = retMayOp
               , nameds     = concat (nameds <$> opNms) ++
                              retNameds }

expr2opNm (If condExpr thenExpr elseExpr) = do
  labelNum <- unName
  let ifCond = AST.Name "if.cond" <> labelNum
      ifThen = AST.Name "if.then" <> labelNum
      ifElse = AST.Name "if.else" <> labelNum
      ifExit = AST.Name "if.exit" <> labelNum

  OpNameds
    { mayOperand  = mayCondOp
    , nameds      = condNameds
    }      <- expr2opNm condExpr
  condOp   <- mayThrowErr ("failed at: " <> tShow condExpr) mayCondOp
  OpNameds
    { mayOperand  = mayIsTrueOp
    , nameds      = isTrueNameds
    }      <- cmp FP.ONE zero condOp
  isTrueOp <- mayThrowErr ("failed comparing:" <> tShow isTrueNameds) mayIsTrueOp
  let
    ifCondNameds = BlkName ifCond:condNameds
                   ++ isTrueNameds
                   ++ [NTerm $ termCondbr isTrueOp ifThen ifElse]

  OpNameds
    { mayOperand = mayThenOp
    , nameds     = thenNameds
    }      <- expr2opNm thenExpr
  thenOp   <- mayThrowErr ("no result: " <> tShow thenNameds) mayThenOp
  let
    ifThenNameds = BlkName ifThen:thenNameds ++ [NTerm $ termBr ifExit]

  OpNameds
    { mayOperand = mayElseOp
    , nameds     = elseNameds
    }      <- expr2opNm elseExpr
  elseOp   <- mayThrowErr ("no result: " <> tShow thenNameds) mayElseOp
  let
    ifElseNameds = BlkName ifElse:elseNameds ++ [NTerm $ termBr ifExit]

  pure defOpNs { mayOperand = Just $ AST.LocalReference double labelNum
               , nameds = ifCondNameds ++
                          ifThenNameds ++
                          ifElseNameds ++
                          [ BlkName ifExit
                          , NInstr $ labelNum :=
                            AST.Phi Type.double [(thenOp, ifThen), (elseOp, ifElse)] []]}







-- Global

defineFunc :: AST.Type -> AST.Name -> [(AST.Type, AST.Name)] -> [AST.BasicBlock]
           -> Maybe AST.Global
defineFunc type' name typedArgs basicBlocks =
  Just $
    AST.functionDefaults { G.returnType  = type'
                         , G.name        = name
                         , G.parameters  = (genParam <$> typedArgs, False)
                         , G.basicBlocks = basicBlocks }


externFunc :: AST.Type -> AST.Name -> [(AST.Type, AST.Name)]
           -> Maybe AST.Global
externFunc type' name typedArgs =
  -- addToDefinitionTable type' name typedArgs
  Just $
    AST.functionDefaults { G.linkage    = Linkage.External
                         , G.returnType = type'
                         , G.name       = name
                         , G.parameters = (genParam <$> typedArgs, False) }


genFunTypePtr :: AST.Type -> AST.Name -> [AST.Type] -> AST.Operand
genFunTypePtr type' name types =
  AST.ConstantOperand $
  Const.GlobalReference
  AST.PointerType { pointerAddrSpace = AddrSpace.AddrSpace 0
                  , pointerReferent =
                      AST.FunctionType { resultType    = type'
                                       , argumentTypes = types
                                       , isVarArg      = False }}
  name


-- Misc (Global)

genParam :: (AST.Type, AST.Name) -> AST.Parameter
genParam (type', name) = G.Parameter type' name []



-- Named Instruction and Operand (OpNameds)

fAdd, fSub, fMul, fDiv :: AST.Operand -> AST.Operand
  -> StateWithErr MetaData OpNameds
fAdd = fOps AST.FAdd
fSub = fOps AST.FSub
fMul = fOps AST.FMul
fDiv = fOps AST.FDiv


cmp :: FP.FloatingPointPredicate -> AST.Operand -> AST.Operand
     -> StateWithErr MetaData OpNameds
cmp fp a b = do
  name <- unName
  pure defOpNs { mayOperand  = Just $ AST.LocalReference double name
                , nameds = [NInstr $ name := AST.FCmp fp a b []] }


ui2fp :: AST.Type -> AST.Operand
     -> StateWithErr MetaData OpNameds
ui2fp type' b = do
  name <- unName
  pure defOpNs { mayOperand  = Just $ AST.LocalReference double name
                , nameds = [NInstr $ name := AST.UIToFP b type' []] }


fCmp fp a b = do
  OpNameds
    { mayOperand = mayOperand
    , nameds     = nameds
    } <- cmp fp a b
  op  <- mayThrowErr "this won't be error" mayOperand
  OpNameds
    { mayOperand = ui2fpOp
    , nameds     = ui2fpNameds
    } <- ui2fp double op
  pure defOpNs { mayOperand = ui2fpOp
               , nameds = nameds ++ ui2fpNameds}


call :: AST.Type -> AST.Operand -> [AST.Operand] -> StateWithErr MetaData OpNameds
call type' fn args = do
  name <- unName
  pure defOpNs { mayOperand = Just $ AST.LocalReference type' name
              , nameds =
                [NInstr $ name := AST.Call Nothing CallC.C [] (Right fn)
                 ((\x -> (x, [])) <$> args) [] []] }

alloca :: AST.Type -> StateWithErr MetaData OpNameds
alloca type' = do
  name <- unName
  pure defOpNs { mayOperand  = Just $ AST.LocalReference (ptrTo type') name
              , nameds = [NInstr $ name := AST.Alloca type' Nothing 0 []] }


storeP :: AST.Operand -> AST.Operand -> OpNameds
storeP addr val = defOpNs { nameds = [NInstr $ AST.Do $ AST.Store False addr val Nothing 0 []] }


load :: AST.Operand -> StateWithErr MetaData OpNameds
load addr@(AST.LocalReference AST.PointerType { pointerReferent = type' } _) = do
  name <- unName
  pure defOpNs { mayOperand = Just $ AST.LocalReference type' name
              , nameds = [NInstr $ name := AST.Load False addr Nothing 0 []]}
load operand = throwErr ("Cannot load non-pointer (Malformed AST): " <> tShow operand)


assign :: AST.Type -> AST.Name -> StateWithErr MetaData OpNameds
assign type' name = do
  OpNameds
    { nameds = allocaNInstrs
    , mayOperand  = mayAllocaOp
    }    <- alloca type'
  addrOp <- mayThrowErr ("failed at assigning (alloca): " <> tShow name) mayAllocaOp
  let storeNInstrs = nameds $ storeP addrOp (AST.LocalReference type' name)
  St.modify $ \meta -> meta { symbolTable = Map.insert name addrOp $ symbolTable meta }
  pure defOpNs { mayOperand  = mayAllocaOp
              , nameds = allocaNInstrs ++ storeNInstrs }


-- Misc (Named Instruction)

fOps :: (AST.FastMathFlags -> AST.Operand -> AST.Operand -> AST.InstructionMetadata -> AST.Instruction)
     -> AST.Operand -> AST.Operand -> StateWithErr MetaData OpNameds
fOps constr a b = do
  name <- unName
  pure defOpNs { mayOperand  = Just $ AST.LocalReference double name
                , nameds = [NInstr $ name := constr AST.noFastMathFlags a b []] }



-- Named Terminator

termRet :: Maybe AST.Operand -> AST.Named AST.Terminator
termRet retOperand = AST.Do $ AST.Ret retOperand []


termBr :: AST.Name -> AST.Named AST.Terminator
termBr dest = AST.Do $ AST.Br dest []


termCondbr :: AST.Operand -> AST.Name -> AST.Name
           -> AST.Named AST.Terminator
termCondbr cond tr fl = AST.Do $ AST.CondBr cond tr fl []



-- Operand

-- LocalReference Type Name
-- ConstantOperand Constant
-- MetadataOperand Metadata

doubleNum :: Double -> StateWithErr MetaData AST.Operand
doubleNum num = pure . AST.ConstantOperand . Const.Float $ Float.Double num
