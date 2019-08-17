{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Parser
  ( pToplevel
  ) where

import           Definition

import           Control.Monad.State        as St
import           Data.Text.Lazy             as T
import           Prelude                    as P
import           Safe                       as Safe
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
lexeme  = L.lexeme spaceConsumer

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

semicolon, comma, colon, dot :: Parser Text
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."

parens, braces, angles, brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")

identifier :: Parser Text
identifier = lexeme . try $ p >>= check
  where
    p       = (\x-> pack . (x:)) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reserved
              then fail $ "keyword " <> show x <> " cannot be an identifier"
              else pure x


pToplevel :: Parser [Expr]
pToplevel = many $ (pFuncDef <|> pExtern) <* semicolon

pFuncDef, pExtern, pFuncCall, pVar, pExpr, pAdd, pMul, pFloat, pInteger :: Parser Expr
pFuncDef = symbol "def" >>
           FuncDef <$> identifier
           <*> parens (many pVar)
           <*> pExpr

pExtern = symbol "extern" >>
          Extern <$> identifier
          <*> parens (many pVar)

pFuncCall = FuncCall <$> identifier
            <*> parens (pExpr `sepBy` colon)


pVar = Var <$> identifier

pExpr = pAdd

pAdd = do
  arg0 <- pMul
  option (arg0) (do op <- choice $ [ pure Plus  <* symbol "+"
                                   , pure Minus <* symbol "-"]
                    arg1 <- pMul
                    pure $ BinOp op arg0 arg1)

pMul = do
  arg0 <- pFactor
  option (arg0) (do op   <- choice $ [ pure Times  <* symbol "*"
                                     , pure Divide <* symbol "/"]
                    arg1 <- pFactor
                    pure $ BinOp op arg0 arg1)

pFloat = Float <$> float

pInteger = Float . fromIntegral <$> integer

pFactor :: Parser Expr
pFactor = choice [ pFloat
                 , pInteger
                 , pExtern
                 , pFuncDef
                 , pFuncCall
                 , pVar
                 , parens pExpr]
