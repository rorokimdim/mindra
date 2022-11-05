module Mindra.Diagrams.Parser.Configuration
  ( parse
  ) where

import Control.Applicative

import Data.Void (Void)
import Text.Megaparsec (choice, runParser)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Error (ParseErrorBundle)

import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import Diagrams.Backend.Rasterific (GifLooping(..))
import Mindra.Diagrams.Types (Configuration(..), Output(..))
import Mindra.Parser.Common (Parser, pStringLiteral, pWhiteSpace)


pSVG :: Parser Configuration
pSVG = do
  _ <- string "SVG"
  pWhiteSpace
  width <- L.decimal
  pWhiteSpace
  height <- L.decimal
  optional pWhiteSpace
  filePath <- optional pStringLiteral
  case filePath of
    Just path ->
      return $ Configuration { _width = width, _height = height, _output = SVGFile path }
    Nothing -> return $ Configuration { _width = width, _height = height, _output = SVGText }

pRaster :: Parser Configuration
pRaster = do
  _ <- string "Raster"
  pWhiteSpace
  width <- L.decimal
  pWhiteSpace
  height <- L.decimal
  pWhiteSpace
  filePath <- pStringLiteral
  return $ Configuration { _width = width, _height = height, _output = RasterFile filePath }

pAnimatedGif :: Parser Configuration
pAnimatedGif = do
  _ <- string "AnimatedGif"
  pWhiteSpace
  width <- L.decimal
  pWhiteSpace
  height <- L.decimal
  pWhiteSpace
  filePath <- pStringLiteral
  pWhiteSpace
  looping <- pLooping
  pWhiteSpace
  delay <- L.decimal
  return $ Configuration
    { _width  = width
    , _height = height
    , _output = AnimatedGifFile filePath looping delay
    }
 where
  pLooping = do
    ltype <- choice [string "LoopingNever", string "LoopingForever", string "LoopingRepeat"]
    case ltype of
      "LoopingNever"   -> return LoopingNever
      "LoopingForever" -> return LoopingForever
      "LoopingRepeat"  -> do
        n <- L.decimal
        return $ LoopingRepeat n

pDiagramsConfiguration :: Parser Configuration
pDiagramsConfiguration = do
  optional pWhiteSpace
  _ <- string "Diagrams"
  pWhiteSpace
  choice [pAnimatedGif, pSVG, pRaster]

parse :: T.Text -> Either (ParseErrorBundle T.Text Void) Configuration
parse = runParser pDiagramsConfiguration "source"
