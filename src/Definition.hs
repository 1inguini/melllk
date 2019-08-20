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
  | Extern   AST.Name [AST.Name]
  deriving (Eq, Ord, Show)

data Op = Plus
        | Minus
        | Times
        | Divide
        deriving (Eq, Ord, Show)

reserved :: [T.Text]
reserved = [ "def", "extern" ]


-- for LLVMGen

type SymbolTable = Map.Map AST.Name AST.Operand

type Names = Map.Map AST.Name Integer

data Code
  = Code
  { basicBlocks :: [AST.BasicBlock]
    -- Basicblocks for Module
  , namedInstrs :: [AST.Named AST.Instruction]
    -- Instructions for BasicBlocks
  -- , blockTerm   :: AST.Named AST.Terminator
    -- Terminater for BasicBlocks
  , symboltable :: SymbolTable
    -- Function scope symbol table
  , instrCount  :: Word
    -- Count of unnamed instructions
  , names       :: Names
    -- Name Supply
  } deriving Show

emptyCode :: Code
emptyCode = Code
  { basicBlocks = []
  , namedInstrs = []
  -- , blockTerm = AST.Do $ AST.Ret Nothing []
  , symboltable = Map.empty
  , instrCount  = 0
  , names       = Map.empty}

entryBlockName :: AST.Name
entryBlockName = genName "entry"

instance Semigroup AST.Name where
  x <> y = AST.Name (unpackName x <> unpackName y)

unpackName :: AST.Name -> BStr.S.ShortByteString
unpackName (AST.Name name)   = name
unpackName (AST.UnName name) = Str.fromString $ show name

-- data Block
--   = Block
--   { index :: Integer
--     -- Block index
--   , stack :: [AST.Named AST.Instruction]
--     -- Stack of instructions
--   , term  :: Maybe (AST.Named AST.Terminator)
--     -- Block terminator
--   } deriving Show

-- emptyBlock :: Block
-- emptyBlock = Block { index = 0
--                    , stack = []
--                    , term = Nothing
--                    }

basicBlockName :: AST.BasicBlock -> AST.Name
basicBlockName (AST.BasicBlock name _ _) = name

renameBBlock :: AST.Name -> AST.BasicBlock -> AST.BasicBlock
renameBBlock name (AST.BasicBlock _ body term) =
  AST.BasicBlock name body term

basicBlockBody :: AST.BasicBlock -> [AST.Named AST.Instruction]
basicBlockBody (AST.BasicBlock _ body _) = body

replacebodyBBlock :: [AST.Named AST.Instruction]
                  -> AST.BasicBlock
                  -> AST.BasicBlock
replacebodyBBlock body (AST.BasicBlock name _ term) =
  AST.BasicBlock name body term

appendInstr2BBlock :: AST.Named AST.Instruction
                   -> AST.BasicBlock
                   -> AST.BasicBlock
appendInstr2BBlock body bblock =
  replacebodyBBlock (body : basicBlockBody bblock) bblock

basicBlockTerm :: AST.BasicBlock -> AST.Named AST.Terminator
basicBlockTerm (AST.BasicBlock _ _ term) = term


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
