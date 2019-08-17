{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Definition where

import           Control.Monad.State as St
import           Data.Text.Lazy      as T
import qualified Data.Void           as V
import           Prelude             as P
import           Text.Megaparsec     as MP

type Name = Text

type Parser = Parsec V.Void Text

data Expr
  = Float Double
  | BinOp Op Expr Expr
  | Var Name
  | FuncCall Name [Expr]
  | FuncDef Name [Expr] Expr
  | Extern Name [Expr]
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)

reserved :: [Text]
reserved = ["def", "extern"]

someFunc :: IO ()
someFunc = putStrLn "Hello"
