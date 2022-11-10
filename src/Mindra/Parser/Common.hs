module Mindra.Parser.Common where

import Control.Applicative

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, manyTill, try)
import Text.Megaparsec.Char (char, space, string)

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

pLineComment :: Parser ()
pLineComment = L.skipLineComment "#"

pBool :: Parser Bool
pBool = do
  b <- choice [string "true", string "false"]
  return $ b == "true"

pFloat :: Parser Float
pFloat = try (L.signed space L.float) <|> L.signed space L.decimal

pDouble :: Parser Double
pDouble = do
  try (L.signed space L.float) <|> L.signed space L.decimal

pList :: Parser a -> Parser [a]
pList p = do
  _ <- char '['
  manyTill p' (char ']')
 where
  p' = do
    optional pWhiteSpace
    v <- p
    optional pWhiteSpace
    return v

pListN :: Parser a -> Int -> Parser [a]
pListN p n = do
  ps <- pList p
  if length ps /= n
    then fail $ "Expected " <> show n <> " values; found " <> show (length ps)
    else return ps

pWhiteSpace :: Parser ()
pWhiteSpace = do
  some $ char ' ' <|> char '\t' <|> char '\n' <|> char ','
  return ()

pRGB :: Parser (Int, Int, Int)
pRGB = do
  r <- L.decimal
  pWhiteSpace
  g <- L.decimal
  pWhiteSpace
  b <- L.decimal
  if maximum [r, g, b] > 255 then fail "Color values must be 0 to 255" else return (r, g, b)

pRGBA :: Parser (Int, Int, Int, Int)
pRGBA = do
  r <- L.decimal
  pWhiteSpace
  g <- L.decimal
  pWhiteSpace
  b <- L.decimal
  pWhiteSpace
  a <- L.decimal
  if maximum [r, g, b, a] > 255 then fail "Color values must be 0 to 255" else return (r, g, b, a)

pStringLiteral :: Parser String
pStringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')
