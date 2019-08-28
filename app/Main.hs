{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Definition
import qualified JIT
import qualified LLVMGen                   as Gen
import qualified Parse

import qualified Control.Monad             as M
import qualified Control.Monad.State       as St
import qualified Control.Monad.Trans       as Trans
import qualified Data.ByteString.Char8     as BStr
import qualified Data.Either               as E
import qualified Data.Maybe                as May
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

-- main :: IO ()
-- main = Hline.runInputT Hline.defaultSettings loop
--   where
--   loop = do
--     minput <- Hline.getInputLine "ready >> "
--     case minput of
--       Nothing    -> Hline.outputStrLn "Goodbye."
--       Just input -> do
--         Trans.liftIO $ eval $ T.pack input
--         loop

-- eval :: T.Text -> IO ()
-- eval input =
--   either (putStrLn . MP.E.errorBundlePretty)
--   (E.either print JIT.runJIT . evalMetaData . Gen.toplevels2module)
--   $ MP.parse (Parse.pToplevel <* MP.eof) "<stdin>" input

initModule :: AST.Module
initModule = AST.defaultModule { AST.moduleName = "my cool jit" }

process :: (AST.Module, MetaData) -> T.Text -> IO (Maybe  AST.Module, MetaData)
process (module_, meta) source = do
  let res = MP.parse (Parse.pToplevel <* MP.eof) "<stdin>" source
  case res of
    Left err -> (putStrLn . MP.E.errorBundlePretty) err >> pure (Nothing, meta)
    Right tops ->
      E.either (\err -> print err >> pure (Nothing, meta))
      (\(module_, meta) -> do { module_ <- JIT.runJIT module_
                              ; pure (module_, meta)})
      (runMetaData $ Gen.toplevels2module (module_, meta) tops)
      -- E.either (const $ pure Nothing) (pure . Just)
      -- (evalMetaData $ Gen.toplevels2module modo tops)

processFile :: String -> IO (Maybe AST.Module, MetaData)
processFile fname = T.IO.readFile fname >>= process (initModule, emptyMetaData)

repl :: IO ()
repl = Hline.runInputT Hline.defaultSettings (loop (initModule, emptyMetaData))
  where
  loop (module_, meta) = do
    minput <- Hline.getInputLine "ready>> "
    case minput of
      Nothing -> Hline.outputStrLn "Goodbye."
      Just input -> do
        mayModuleMeta <- Trans.liftIO $ process (module_, meta) $ T.pack input
        case mayModuleMeta of
          (Just newModule, meta) -> loop (newModule, meta)
          (Nothing, meta)        -> loop (module_, meta)

rppl :: IO ()
rppl = Hline.runInputT Hline.defaultSettings loop
  where
  loop = do
    minput <- Hline.getInputLine "ready>> "
    case minput of
      Nothing -> Hline.outputStrLn "Goodbye."
      Just input -> do
        E.either
          (Trans.lift . putStrLn . MP.E.errorBundlePretty)
          (Trans.lift . PrettyS.pPrint)
          $ MP.parse (Parse.pToplevel <* MP.eof) "<stdin>" (T.pack input)
        loop



main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    []     -> repl
    -- []     -> rppl
    fnames -> processFile `mapM_` fnames

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
