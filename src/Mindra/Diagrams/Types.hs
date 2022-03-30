module Mindra.Diagrams.Types where

data Output = SVGText | SVGFile FilePath | RasterFile FilePath deriving (Eq, Show)

data Configuration = Configuration { _width :: Int
                                   , _height :: Int
                                   , _output :: Output
                                   } deriving (Show)
