module Mindra.Gloss.Parser.Configuration
  ( parse
  ) where

import Control.Applicative

import Data.Void (Void)
import Graphics.Gloss.Data.Color (makeColorI)
import Text.Megaparsec (choice, eof, runParser)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Error (ParseErrorBundle)

import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import Mindra.Gloss.Types (Configuration(..), Mode(..))
import Mindra.Parser.Common (Parser, pStringLiteral, pWhiteSpace)

defaultConfiguration = Configuration
  { _windowWidth     = 512
  , _windowHeight    = 512
  , _windowX         = 10
  , _windowY         = 10
  , _windowTitle     = "Mindra"
  , _fullScreen      = False
  , _mode            = Static
  , _stepsPerSecond  = 50
  , _backgroundColor = makeColorI 255 255 255 255
  , _noEvent         = False
  , _noStep          = False
  }

pWindow :: Configuration -> Parser Configuration
pWindow cfg = do
  optional pWhiteSpace
  _ <- string "Window"
  pWhiteSpace
  width <- L.decimal
  pWhiteSpace
  height <- L.decimal
  pWhiteSpace
  x <- L.decimal
  pWhiteSpace
  y <- L.decimal
  pWhiteSpace
  title <- pStringLiteral
  return $ cfg
    { _windowWidth  = width
    , _windowHeight = height
    , _windowX      = x
    , _windowY      = y
    , _windowTitle  = title
    }

pFullScreen :: Configuration -> Parser Configuration
pFullScreen cfg = do
  optional pWhiteSpace
  _ <- string "FullScreen"
  return $ cfg { _fullScreen = True }

pMode :: Configuration -> Parser Configuration
pMode cfg = do
  optional pWhiteSpace
  _ <- string "Mode"
  pWhiteSpace
  mode <- choice [char '0', char '1']
  if mode == '0' then return (cfg { _mode = Static }) else return (cfg { _mode = Interactive })

pStepsPerSecond :: Configuration -> Parser Configuration
pStepsPerSecond cfg = do
  optional pWhiteSpace
  _ <- string "StepsPerSecond"
  pWhiteSpace
  sps <- L.decimal
  return $ cfg { _stepsPerSecond = sps }

pColor :: Configuration -> Parser Configuration
pColor cfg = do
  optional pWhiteSpace
  _ <- string "Color"
  pWhiteSpace
  r <- L.decimal
  pWhiteSpace
  g <- L.decimal
  pWhiteSpace
  b <- L.decimal
  pWhiteSpace
  a <- L.decimal
  return $ cfg { _backgroundColor = makeColorI r g b a }

pNoEvent :: Configuration -> Parser Configuration
pNoEvent cfg = do
  optional pWhiteSpace
  _ <- string "NoEvent"
  return $ cfg { _noEvent = True }

pNoStep :: Configuration -> Parser Configuration
pNoStep cfg = do
  optional pWhiteSpace
  _ <- string "NoStep"
  return $ cfg { _noStep = True }

pEmpty :: Configuration -> Parser Configuration
pEmpty cfg = do
  eof
  return cfg

pConfiguration :: Configuration -> Parser Configuration
pConfiguration cfg = do
  optional pWhiteSpace
  empty <- optional $ pEmpty cfg
  case empty of
    Just finalCfg -> return finalCfg
    Nothing       -> do
      newCfg <- choice
        [ pColor cfg
        , pMode cfg
        , pWindow cfg
        , pFullScreen cfg
        , pStepsPerSecond cfg
        , pNoEvent cfg
        , pNoStep cfg
        ]
      pConfiguration newCfg

pGlossConfiguration :: Parser Configuration
pGlossConfiguration = do
  optional pWhiteSpace
  _ <- string "Gloss"
  pConfiguration defaultConfiguration

parse :: T.Text -> Either (ParseErrorBundle T.Text Void) Configuration
parse = runParser pGlossConfiguration "source"
