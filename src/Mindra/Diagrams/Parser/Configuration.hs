module Mindra.Diagrams.Parser.Configuration
  ( parse
  )
where

import Control.Applicative
import Control.Monad

import Data.Void (Void)
import Graphics.Gloss.Data.Color (makeColorI)
import Text.Megaparsec (choice, optional, eof, runParser)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Error (ParseErrorBundle)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec.Char.Lexer as L

import Mindra.Diagrams.Types (Configuration(..))
import Mindra.Parser.Common (Parser, pWhiteSpace, pStringLiteral)

pSVG :: Parser Configuration
pSVG = do
  _ <- string "SVG"
  pWhiteSpace
  width <- L.decimal
  pWhiteSpace
  height <- L.decimal
  return $ Configuration { _width = width, _height = height }

pDiagramsConfiguration :: Parser Configuration
pDiagramsConfiguration = do
  optional pWhiteSpace
  _ <- string "Diagrams"
  pWhiteSpace
  pSVG

parse :: T.Text -> Either (ParseErrorBundle T.Text Void) Configuration
parse = runParser pDiagramsConfiguration "source"
