{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Definition where

import qualified Data.ByteString.Short as BStr.S
import qualified Data.Map              as Map
import qualified Data.String           as Str
import qualified Data.Text             as T
import qualified Data.Void             as Void

import qualified Text.Megaparsec       as MP

import qualified LLVM.AST              as AST

-- for Parser

type Parser = MP.Parsec Void.Void T.Text

data Expr
  = Float    Double
  | BinOp    Op       Expr   Expr
  | Var      AST.Name
  | FuncCall AST.Name [Expr]
  | FuncDef  AST.Name [AST.Name] Expr
  | Extern   AST.Name [Expr]
  deriving (Eq, Ord, Show)

data Op = Plus
        | Minus
        | Times
        | Divide
        deriving (Eq, Ord, Show)

reserved :: [T.Text]
reserved = [ "def", "extern" ]


-- for LLVMGen

type SymbolTable = Map.Map T.Text AST.Operand

type Names = Map.Map AST.Name Integer

data Code
  = Code
  { -- currentBlock :: AST.Name
    -- -- Name of the active block to append to
  -- ,
  blocks        :: Map.Map AST.Name Block
    -- Blocks for function
  , symboltable :: SymbolTable
    -- Function scope symbol table
  , blockCount  :: Integer
    -- Count of basic blocks
  , instrCount  :: Word
    -- Count of unnamed instructions
  , names       :: Names
    -- Name Supply
  } deriving Show

emptyCode :: Code
emptyCode = Code
  { blocks      = Map.singleton entryBlockName
                  emptyBlock
  , symboltable = Map.empty
  , blockCount  = 1
  , instrCount  = 0
  , names       = Map.empty}

entryBlockName :: AST.Name
entryBlockName = genName "entry"

instance Semigroup AST.Name where
  x <> y = AST.Name (unpackName x <> unpackName y)

unpackName :: AST.Name -> BStr.S.ShortByteString
unpackName (AST.Name name)   = name
unpackName (AST.UnName name) = Str.fromString $ show name

data Block
  = Block
  { index :: Integer
    -- Block index
  , stack :: [AST.Named AST.Instruction]
    -- Stack of instructions
  , term  :: Maybe (AST.Named AST.Terminator)
    -- Block terminator
  } deriving Show

emptyBlock :: Block
emptyBlock = Block { index = 0
                   , stack = []
                   , term = Nothing
                   }

double :: AST.Type
double = AST.FloatingPointType AST.DoubleFP

-- Text
tShow :: Show a => a -> T.Text
tShow = T.pack . show

fromText :: Str.IsString a => T.Text -> a
fromText = Str.fromString . T.unpack

genName :: T.Text -> AST.Name
genName = AST.Name . fromText

nameShow :: Show a => a -> AST.Name
nameShow = AST.mkName . show
