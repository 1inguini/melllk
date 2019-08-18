{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module MelllkParser (
  pToplevel) where

import           Definition

import           Control.Monad.State        as St
import           Data.Text                  as T
import           Prelude                    as P
import           Safe
import           Text.Megaparsec            as MP
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Debug      as MP.Debug

spaceConsumer :: Parser ()
spaceConsumer = L.space
                space1
                (L.skipLineComment "//")
                (L.skipBlockComment "/*" "*/")

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = try . L.lexeme spaceConsumer

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

semicolon, comma, colon, period :: Parser Text
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
period    = symbol "."

parens, braces, angles, brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")

identifier :: Parser Text
identifier = lexeme $
  (\x-> pack . (x:))
  <$> letterChar
  <*> many alphaNumChar >>= check
  where
    check x = if x `elem` reserved
              then fail $ "keyword " <> show x <> " cannot be an identifier"
              else pure x

pToplevel :: Parser [Expr]
pToplevel = many $ pFactor <* semicolon

pFactor :: Parser Expr
pFactor = choice [ pExtern
                 , pFuncDef
                 , pFuncCall
                 , pExpr]

pFuncDef, pExtern, pFuncCall, pVar, pExpr, pAdd, pMul, pFloat, pInteger :: Parser Expr
pExtern = symbol "extern" >>
          Extern <$> identifier
          <*> parens (many pVar)

pFuncDef = symbol "def" >>
           FuncDef <$> identifier
           <*> parens (many pVar)
           <*> pExpr

pFuncCall = try $ FuncCall
            <$> identifier
            <*> parens (pExpr `sepBy` comma)

pExpr = pAdd

pAdd = do
  arg0 <- pMul
  option arg0 (BinOp
                <$> choice [ Plus  <$ symbol "+"
                           , Minus <$ symbol "-"]
                <*> pure arg0
                <*> pAdd)

pMul = do
  arg0 <- pTerm
  option arg0 (BinOp
                <$> choice [ Times  <$ symbol "*"
                           , Divide <$ symbol "/"]
                <*> pure arg0
                <*> pMul)

pTerm :: Parser Expr
pTerm = choice [ pVar
               , pFloat
               , pInteger
               , parens pExpr]

pVar = Var <$> identifier

pFloat = Float <$> float

pInteger = Float . fromIntegral <$> integer
