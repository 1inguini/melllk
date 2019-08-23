{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Definition
import qualified JIT
import qualified LLVMGen                   as Gen
import qualified Parse

import qualified Control.Monad.State       as St
import qualified Control.Monad.Trans       as Trans
import qualified Data.ByteString.Char8     as BStr
import qualified Data.Either               as E
import qualified Data.String               as Str
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T.IO
import qualified Data.Text.Lazy            as T.Lazy
import qualified Safe
import qualified System.Console.Haskeline  as Hline
import qualified System.Environment        as Env
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

import qualified LLVM.IRBuilder            as IR

-- eval :: T.Text -> IO BStr.ByteString
-- eval input =
--   either
--   (\err -> pure $ Str.fromString . MP.E.errorBundlePretty $ err)
--   (\expr -> E.either (pure . Str.fromString . T.unpack) Gen.genLLVM
--             $ St.execStateT (Gen.codegenTop expr) AST.defaultModule)
--   $ MP.parse (Parse.pToplevel <* MP.eof) "<stdin>" input

-- eval :: T.Text -> IO BStr.ByteString
-- eval input =
--   either
--   (\err -> pure $ Str.fromString . MP.E.errorBundlePretty $ err)
--   (pure . Str.fromString . T.Lazy.unpack . PrettyS.pShow)
--   $ MP.parse (Parse.pToplevel <* MP.eof) "<stdin>" input

-- main :: IO ()
-- main = Hline.runInputT Hline.defaultSettings loop
--   where
--   loop = do
--     minput <- Hline.getInputLine "ready >> "
--     case minput of
--       Nothing    -> Hline.outputStrLn "Goodbye."
--       Just input -> do
--         code <- Trans.liftIO $ eval $ T.pack input
--         Trans.liftIO $ BStr.putStrLn code
--         loop

main :: IO ()
main = Hline.runInputT Hline.defaultSettings loop
  where
  loop = do
    minput <- Hline.getInputLine "ready >> "
    case minput of
      Nothing    -> Hline.outputStrLn "Goodbye."
      Just input -> do
        Trans.liftIO $ eval $ T.pack input
        loop

eval :: T.Text -> IO ()
eval input =
  either (putStrLn . MP.E.errorBundlePretty)
  (\tops -> E.either print
            (\astModule -> JIT.runJIT $ astModule)
            $ evalMetaData (Gen.toplevels2module tops))
  $ MP.parse (Parse.pToplevel <* MP.eof) "<stdin>" input


-- int :: AST.Type
-- int = AST.IntegerType 32

-- defAdd :: AST.Definition
-- defAdd = AST.GlobalDefinition AST.functionDefaults
--   { G.name = AST.Name "add"
--   , G.parameters =
--       ( [ G.Parameter int (AST.Name "a") []
--         , G.Parameter int (AST.Name "b") [] ]
--       , False )
--   , G.returnType = int
--   , G.basicBlocks = [body]
--   }
--   where
--     body = AST.BasicBlock
--         (AST.Name "entry")
--         [ AST.Name "c" :=
--             AST.Add False  -- no signed wrap
--                 False  -- no unsigned wrap
--                 (AST.LocalReference int (AST.Name "a"))
--                 (AST.LocalReference int (AST.Name "b"))
--                 []
--         , AST.Name "result" :=
--             AST.Add False  -- no signed wrap
--                 False  -- no unsigned wrap
--                 (AST.LocalReference int (AST.Name "a"))
--                 (AST.LocalReference int (AST.Name "c"))
--                 []]
--         (AST.Do $ AST.Ret (Just (AST.LocalReference int (AST.Name "result"))) [])

-- module_ :: AST.Module
-- module_ = AST.defaultModule
--   { AST.moduleName = "main"
--   , AST.moduleDefinitions = [defAdd]
--   }

-- main :: IO ()
-- main = BStr.putStrLn =<< Gen.genLLVM module_
