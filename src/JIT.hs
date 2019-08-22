{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module JIT where

import qualified Control.Exception               as Exception
import           Control.Monad                   ((<=<), (>=>))
import qualified Control.Monad                   as M
import qualified Control.Monad.State             as St
import qualified Control.Monad.Trans             as Trans

import qualified Data.ByteString.Char8           as BStr
import qualified Data.ByteString.Short           as BStr.S
import qualified Data.Either                     as E
import qualified Data.Function                   as Func
import qualified Data.List                       as Ls
import           Data.Map                        ((!?))
import qualified Data.Map                        as Map
import qualified Data.Maybe                      as May
import qualified Data.String                     as Str
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T.IO
import qualified Data.Text.Lazy                  as T.Lazy

import qualified Foreign.Ptr                     as Ptr

import qualified Safe

import qualified Text.Pretty.Simple              as PrettyS

import           LLVM.AST                        (Named ((:=)))
import qualified LLVM.AST                        as AST
import qualified LLVM.AST.CallingConvention      as CallC
import qualified LLVM.AST.Constant               as Const
import qualified LLVM.AST.Float                  as Float
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Global                 as G
import qualified LLVM.AST.IntegerPredicate       as IP
import qualified LLVM.AST.Type                   as Type

import qualified LLVM
import qualified LLVM.Analysis                   as Analysis
import qualified LLVM.Context                    as Context
import qualified LLVM.Exception                  as Exception
import qualified LLVM.ExecutionEngine            as ExecEngine
import qualified LLVM.PassManager                as PassMng
import qualified LLVM.Target                     as Target

runJIT :: AST.Module -> IO ()
runJIT module_ = Context.withContext
  (\context -> jit context $ \executionEngine -> do
  Target.initializeAllTargets
  LLVM.withModuleFromAST context module_ $ \module_ ->
      PassMng.withPassManager passes $ \passManager -> do
        _ <- PassMng.runPassManager passManager module_
        Analysis.verify module_
        LLVM.moduleLLVMAssembly module_ >>= BStr.putStrLn
        ExecEngine.withModuleInEngine executionEngine module_ $ \ee -> do
          mainfn <- ExecEngine.getFunction ee (AST.Name "main")
          case mainfn of
            Just fn -> do
              res <- run fn
              putStrLn $ ">>> Evaluated to: " ++ show res
            Nothing -> return ())
  `Exception.catch` (\(Exception.EncodeException err) ->
                       T.IO.putStrLn . T.Lazy.toStrict . PrettyS.pString $ err)

passes :: PassMng.PassSetSpec
passes = PassMng.defaultCuratedPassSetSpec { PassMng.optLevel = Just 3 }

jit :: Context.Context -> (ExecEngine.MCJIT -> IO a) -> IO a
jit context = ExecEngine.withMCJIT context optlevel model ptrElim fastInstr
  where
    optlevel   = Just 2  -- optimization level
    model      = Nothing -- code model ( Default )
    ptrElim    = Nothing -- frame pointer elimination
    fastInstr  = Nothing -- fast instruction selection

foreign import ccall "dynamic" haskFun :: Ptr.FunPtr (IO Double) -> (IO Double)

run :: Ptr.FunPtr a -> IO Double
run fn = haskFun (Ptr.castFunPtr fn :: Ptr.FunPtr (IO Double))
