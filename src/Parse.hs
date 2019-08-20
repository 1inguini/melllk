{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Parse (
  pToplevel
  ) where

import           Definition

import qualified Data.Text                  as T

import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MP.Ch
import qualified Text.Megaparsec.Char.Lexer as MP.L
import qualified Text.Megaparsec.Debug      as MP.Debug
import qualified Text.Megaparsec.Error      as MP.E

spaceConsumer :: Parser ()
spaceConsumer = MP.L.space
                MP.Ch.space1
                (MP.L.skipLineComment "//")
                (MP.L.skipBlockComment "/*" "*/")

symbol :: T.Text -> Parser T.Text
symbol = MP.L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = MP.try . MP.L.lexeme spaceConsumer

integer :: Parser Integer
integer = lexeme MP.L.decimal

float :: Parser Double
float = lexeme MP.L.float

semicolon, comma, colon, period :: Parser T.Text
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
period    = symbol "."

parens, braces, angles, brackets :: Parser a -> Parser a
parens    = MP.between (symbol "(") (symbol ")")
braces    = MP.between (symbol "{") (symbol "}")
angles    = MP.between (symbol "<") (symbol ">")
brackets  = MP.between (symbol "[") (symbol "]")

identifier :: Parser T.Text
identifier = lexeme $
  (\x-> T.pack . (x:))
  <$> MP.Ch.letterChar
  <*> MP.many MP.Ch.alphaNumChar >>= check
  where
    check x = if x `elem` reserved
              then fail $ "keyword " <> show x <> " cannot be an identifier"
              else pure x

name = genName <$> identifier

pToplevel :: Parser [Expr]
pToplevel = MP.many $ pFactor <* semicolon

pFactor :: Parser Expr
pFactor = MP.choice [ pExtern
                    , pFuncDef
                    , pFuncCall
                    , pExpr]

pFuncDef, pExtern, pFuncCall, pVar, pExpr, pAdd, pMul, pFloat, pInteger :: Parser Expr
pExtern = symbol "extern" >>
          Extern <$> name
          <*> parens (MP.many name)

pFuncDef = symbol "def" >>
           FuncDef <$> name
           <*> parens (MP.many name)
           <*> pExpr

pFuncCall = MP.try $ FuncCall
            <$> name
            <*> parens (pExpr `MP.sepBy` comma)

pExpr = pAdd

pAdd = do
  arg0 <- pMul
  MP.option arg0 (BinOp
                  <$> MP.choice [ Plus  <$ symbol "+"
                                , Minus <$ symbol "-"]
                  <*> pure arg0
                  <*> pAdd)

pMul = do
  arg0 <- pTerm
  MP.option arg0 (BinOp
                  <$> MP.choice [ Times  <$ symbol "*"
                                , Divide <$ symbol "/"]
                  <*> pure arg0
                  <*> pMul)

pTerm :: Parser Expr
pTerm = MP.choice [ pVar
                  , pFloat
                  , pInteger
                  , parens pExpr]

pVar = Var <$> name

pFloat = Float <$> float

pInteger = Float . fromIntegral <$> integer
