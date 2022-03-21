module Mindra.Diagrams.Parser
  ( parseConfiguration
  , parseSVG
  )
where

import qualified Mindra.Diagrams.Parser.Configuration as Configuration (parse)
import qualified Mindra.Diagrams.Parser.SVG as SVG (parse)

parseConfiguration = Configuration.parse
parseSVG = SVG.parse
