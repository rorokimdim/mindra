module Mindra.Parser.Configuration where

import qualified Data.Text as T

import qualified Mindra.Gloss.Parser as GP
import qualified Mindra.Diagrams.Parser as DP
import qualified Mindra.Gloss.Types as GT (Configuration(..))
import qualified Mindra.Diagrams.Types as DT (Configuration(..))

import Text.Megaparsec.Error (errorBundlePretty)

data MindraConfiguration = GlossConfiguration GT.Configuration | DiagramsConfiguration DT.Configuration deriving (Show)

firstWord :: T.Text -> T.Text
firstWord body = if null xs then "" else head xs where xs = T.words $ T.strip body

parseConfiguration :: T.Text -> Either T.Text MindraConfiguration
parseConfiguration body = parseConfiguration' (firstWord body) body
 where
  parseConfiguration' :: T.Text -> T.Text -> Either T.Text MindraConfiguration
  parseConfiguration' "Gloss" body = case GP.parseConfiguration body of
    Left  err -> Left $ T.pack (errorBundlePretty err)
    Right cfg -> Right $ GlossConfiguration cfg
  parseConfiguration' "Diagrams" body = case DP.parseConfiguration body of
    Left  err -> Left $ T.pack (errorBundlePretty err)
    Right cfg -> Right $ DiagramsConfiguration cfg
  parseConfiguration' k body = Left ("Expected Gloss or Diagrams; found '" <> k <> "'")
