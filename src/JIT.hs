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
import qualified Data.Void                       as Void

import qualified Safe

import qualified System.Console.Haskeline        as Hline
import qualified System.Environment              as Env

import qualified Text.Megaparsec                 as MP
import qualified Text.Megaparsec.Char            as MP.Ch
import qualified Text.Megaparsec.Char.Lexer      as MP.L
import qualified Text.Megaparsec.Debug           as MP.Debug
import qualified Text.Megaparsec.Error           as MP.E
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
import qualified LLVM.Context                    as Context
import qualified LLVM.Exception                  as Exception
import qualified LLVM.PassManager                as PassMng
import qualified LLVM.Target                     as Target

runJIT :: AST.Module -> IO ()
runJIT module_ = (Context.withContext $ \context -> do
  Target.initializeAllTargets
  LLVM.withModuleFromAST context module_ $ \module_ ->
      PassMng.withPassManager passes $ \passManager -> do
        _ <- PassMng.runPassManager passManager module_
        LLVM.moduleLLVMAssembly module_ >>= BStr.putStrLn)
  `Exception.catch` (\err -> PrettyS.pPrint (err :: Exception.EncodeException))

passes :: PassMng.PassSetSpec
passes = PassMng.defaultCuratedPassSetSpec { PassMng.optLevel = Just 3 }
