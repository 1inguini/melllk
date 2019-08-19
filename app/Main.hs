{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Definition
import qualified LLVMGen                   as Gen
import qualified Parse

import qualified Data.ByteString.Char8     as BStr
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as T.Lazy
import qualified Text.Megaparsec           as MP
import qualified Text.Megaparsec.Error     as MP.E
import qualified Text.Pretty.Simple        as PrettyS

import           LLVM.AST                  (Named ((:=)))
import qualified LLVM.AST                  as AST
import qualified LLVM.AST.Constant         as Const
import qualified LLVM.AST.Float            as Float
import qualified LLVM.AST.Global           as G
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type             as Type
import qualified LLVM.Context              as Context
import qualified LLVM.Prelude              as LP

eval :: T.Text -> Either T.Text T.Text
eval input =
  either
  (Right . T.pack . MP.E.errorBundlePretty)
  (Left . T.Lazy.toStrict . PrettyS.pShow)
  $ MP.parse (Parse.pToplevel <* MP.eof) "<stdin>" input

-- main :: IO ()
-- main = runInputT defaultSettings loop
--   where
--   loop = do
--     minput <- getInputLine "ready >> "
--     case minput of
--       Nothing    -> outputStrLn "Goodbye."
--       Just input -> either
--                     (\x -> do
--                         liftIO $ putStrLn (unpack x)
--                         loop)
--                     (\err -> do
--                         liftIO $ putStrLn (unpack err)
--                         loop)
--                     $ eval (pack input)

-- import           LLVM.AST
-- import qualified LLVM.AST              as AST
-- import           LLVM.AST.Global
-- import           LLVM.Context
-- import           LLVM.Module

-- import           LLVM.IRBuilder

int :: AST.Type
int = AST.IntegerType 32

defAdd :: AST.Definition
defAdd = AST.GlobalDefinition AST.functionDefaults
  { G.name = AST.Name "add"
  , G.parameters =
      ( [ G.Parameter int (AST.Name "a") []
        , G.Parameter int (AST.Name "b") [] ]
      , False )
  , G.returnType = int
  , G.basicBlocks = [body]
  }
  where
    body = G.BasicBlock
        (AST.Name "entry")
        [ AST.Name "c" :=
            AST.Add False  -- no signed wrap
                False  -- no unsigned wrap
                (AST.LocalReference int (AST.Name "a"))
                (AST.LocalReference int (AST.Name "b"))
                []
        , AST.Name "result" :=
            AST.Add False  -- no signed wrap
                False  -- no unsigned wrap
                (AST.LocalReference int (AST.Name "a"))
                (AST.LocalReference int (AST.Name "c"))
                []]
        (AST.Do $ AST.Ret (Just (AST.LocalReference int (AST.Name "result"))) [])

module_ :: AST.Module
module_ = AST.defaultModule
  { AST.moduleName = "main"
  , AST.moduleDefinitions = [defAdd]
  }

main :: IO ()
main = BStr.putStrLn =<< Gen.genLLVM module_
