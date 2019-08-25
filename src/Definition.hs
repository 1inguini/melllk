{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Definition where

import qualified Control.Monad.State   as St
import qualified Data.ByteString.Short as BStr.S
import qualified Data.Map              as Map
import qualified Data.String           as Str
import qualified Data.Text             as T
import qualified Data.Void             as Void

import qualified Text.Megaparsec       as MP

import           LLVM.AST              (Named ((:=)))
import qualified LLVM.AST              as AST
import qualified LLVM.AST.Type         as Type

-- for Parser

type Parser = MP.Parsec Void.Void T.Text

type SByteStr = BStr.S.ShortByteString

data Toplevel = FuncDef  AST.Name [AST.Name] Expr
              | Extern   AST.Name [AST.Name]
              | Expr     Expr

data Expr
  = Float    Double
  | BinOp    Op       Expr   Expr
  | Var      AST.Name
  | FuncCall AST.Name [Expr]
  deriving (Eq, Ord, Show)

data Op = Plus
        | Minus
        | Times
        | Divide
        | LsT
        deriving (Eq, Ord, Show)

reserved :: [T.Text]
reserved = [ "def", "extern" ]


-- for LLVMGen

type SymbolTable = Map.Map AST.Name AST.Operand

type DefinitionTable = Map.Map AST.Name AST.Operand

type Names = Map.Map AST.Name Integer

data NInstrsOperand =
  NIO { mayOperand  :: Maybe AST.Operand
      , namedInstrs :: [AST.Named AST.Instruction] }

defNIO :: NInstrsOperand
defNIO = NIO { mayOperand  = Nothing
             , namedInstrs = []}

data MetaData
  = MetaData
  { symbolTable     :: SymbolTable
    -- Function scope symbol table
  , definitionTable :: DefinitionTable
    -- Global scope symbol table
  , unusedNum       :: Word
    -- Count of unnamed instructions
  , names           :: Names
    -- Name Supply
  } deriving Show

execMetaData :: StateWithErr MetaData a -> Either T.Text MetaData
execMetaData m = St.execStateT m emptyMetaData

evalMetaData :: StateWithErr MetaData a -> Either T.Text a
evalMetaData m = St.evalStateT m emptyMetaData

runMetaData :: StateWithErr MetaData a -> Either T.Text (a, MetaData)
runMetaData m = St.runStateT m emptyMetaData

emptyMetaData :: MetaData
emptyMetaData = MetaData
  { -- namedInstrs     = []
  -- ,
    symbolTable     = Map.empty
  , definitionTable = Map.empty
  , unusedNum       = 0
  , names           = Map.empty}

entryBlockName :: AST.Name
entryBlockName = genName "entry"

instance Semigroup AST.Name where
  x <> y = AST.Name (unpackName x <> unpackName y)

unpackName :: AST.Name -> BStr.S.ShortByteString
unpackName (AST.Name name)   = name
unpackName (AST.UnName name) = Str.fromString $ show name

type StateWithErr a b = St.StateT a (Either T.Text) b

throwErr :: T.Text -> StateWithErr a b
throwErr = St.lift . Left

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
