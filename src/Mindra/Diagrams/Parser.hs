module Mindra.Diagrams.Parser
  ( parseConfiguration
  , parseAnimatedGif
  , parseRasterific
  , parseSVG
  ) where

import qualified Mindra.Diagrams.Parser.Configuration as Configuration (parse)
import qualified Mindra.Diagrams.Parser.Generic2D as Generic2D
  (parseAnimatedGif, parseRasterific, parseSVG)

parseConfiguration = Configuration.parse
parseSVG = Generic2D.parseSVG
parseRasterific = Generic2D.parseRasterific
parseAnimatedGif = Generic2D.parseAnimatedGif
