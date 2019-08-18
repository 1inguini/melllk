{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Definition where

import           Control.Monad.State as St
import           Data.Map
import           Data.String
import           Data.Text           as T
import qualified Data.Void           as V
import           Prelude             as P
import           Text.Megaparsec     as MP

import           LLVM.AST

-- for Parser

type Parser = Parsec V.Void Text

data Expr
  = Float    Double
  | BinOp    Op      Expr   Expr
  | Var      Text
  | FuncCall Text [Expr]
  | FuncDef  Text [Expr] Expr
  | Extern   Text [Expr]
  deriving (Eq, Ord, Show)

data Op = Plus
        | Minus
        | Times
        | Divide
        deriving (Eq, Ord, Show)

reserved :: [Text]
reserved = [ "def", "extern" ]


-- for LLVMGen

type SymbolTable = Map Text Operand

type Names = Map Text Int

data CodegenState
  = CodegenState
  { currentBlock :: Name
    -- Name of the active block to append to
  , blocks       :: Map Name BlockState
    -- Blocks for function
  , symtab       :: SymbolTable
    -- Function scope symbol table
  , blockCount   :: Integer
    -- Count of basic blocks
  , count        :: Word
    -- Count of unnamed instructions
  , names        :: Names
    -- Name Supply
  } deriving Show

data BlockState
  = BlockState
  { index :: Integer
    -- Block index
  , stack :: [Named Instruction]
    -- Stack of instructions
  , term  :: Maybe (Named Terminator)
    -- Block terminator
  } deriving Show

double :: Type
double = FloatingPointType DoubleFP

-- Text
tShow :: Show a => a -> Text
tShow = pack . show

fromText :: IsString a => Text -> a
fromText = fromString . unpack
