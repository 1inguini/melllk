{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Definition
import           Parser

import           Data.Text.Lazy        as T
import           Safe                  as Safe
import           System.Environment    as Env
import           Text.Megaparsec       as MP
import qualified Text.Megaparsec.Error as MP.E
import qualified Text.Pretty.Simple    as PrettyS

main :: IO ()
main = do
  input <- pack . Safe.headDef "" <$> Env.getArgs
  case parse pToplevel "" input of
    Right parsed -> do
      putStrLn . unpack . PrettyS.pShow
        $ parsed
      -- case program2nasm parsed of
      --   Right nasm ->
      --     mapM_ (putStrLn . unpack) $ nasm
      --   Left err   ->
      --     putStrLn . unpack . PrettyS.pShow
      --     $ err
    Left err  ->
      putStrLn $ MP.E.errorBundlePretty err
