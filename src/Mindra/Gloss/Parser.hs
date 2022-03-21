module Mindra.Gloss.Parser
  ( parseConfiguration
  , parsePicture
  )
where

import qualified Mindra.Gloss.Parser.Configuration as Configuration (parse)
import qualified Mindra.Gloss.Parser.Picture as Picture (parse)

parseConfiguration = Configuration.parse
parsePicture = Picture.parse
