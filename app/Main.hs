{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Definition
import           MelllkParser

import           Control.Monad.Trans
import           Data.Text                as T
import qualified Data.Text.IO             as T.IO
import qualified Data.Text.Lazy           as T.Lazy
import           Safe
import           System.Console.Haskeline
import           System.Environment       as Env
import           Text.Megaparsec          as MP
import qualified Text.Megaparsec.Error    as MP.E
import qualified Text.Pretty.Simple       as PrettyS

eval :: Text -> Either Text Text
eval input =
  either
  (Right . pack . MP.E.errorBundlePretty)
  (Left . T.Lazy.toStrict . PrettyS.pShow)
  $ parse (pToplevel <* eof) "<stdin>" input


main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready >> "
    case minput of
      Nothing    -> outputStrLn "Goodbye."
      Just input -> either
                    (\x -> do
                        liftIO $ putStrLn (unpack x)
                        loop)
                    (\err -> do
                        liftIO $ putStrLn (unpack err)
                        loop)
                    $ eval (pack input)

-- import           LLVM.AST
-- import qualified LLVM.AST              as AST
-- import           LLVM.AST.Global
-- import           LLVM.Context
-- import           LLVM.Module

-- import           LLVM.IRBuilder

-- import           Data.ByteString.Char8 as BS

-- int :: Type
-- int = IntegerType 32

-- defAdd :: Definition
-- defAdd = GlobalDefinition functionDefaults
--   { name = Name "add"
--   , parameters =
--       ( [ Parameter int (Name "a") []
--         , Parameter int (Name "b") [] ]
--       , False )
--   , returnType = int
--   , basicBlocks = [body]
--   }
--   where
--     body = BasicBlock
--         (Name "entry")
--         [ Name "c" :=
--             Add False  -- no signed wrap
--                 False  -- no unsigned wrap
--                 (LocalReference int (Name "a"))
--                 (LocalReference int (Name "b"))
--                 []
--         , Name "result" :=
--             Add False  -- no signed wrap
--                 False  -- no unsigned wrap
--                 (LocalReference int (Name "a"))
--                 (LocalReference int (Name "c"))
--                 []]
--         (Do $ Ret (Just (LocalReference int (Name "result"))) [])

-- module_ :: AST.Module
-- module_ = defaultModule
--   { moduleName = "main"
--   , moduleDefinitions = [defAdd]
--   }

-- genLLVM :: AST.Module -> IO ByteString
-- genLLVM mod = withContext $ \ctx ->
--   withModuleFromAST ctx mod moduleLLVMAssembly

-- main :: IO ()
-- main = BS.putStrLn =<< genLLVM module_
