module Mindra.Diagrams.Types where

import Diagrams.Backend.Rasterific (GifDelay, GifLooping(..))

instance Show GifLooping where
  show LoopingNever      = "LoopingNever"
  show LoopingForever    = "LoopingForever"
  show (LoopingRepeat x) = "LoopingRepeat" <> show x

instance Eq GifLooping where
  LoopingNever    == LoopingNever    = True
  LoopingForever  == LoopingForever  = True
  LoopingRepeat x == LoopingRepeat y = x == y
  _               == _               = False

data Output = SVGText | SVGFile FilePath | RasterFile FilePath | AnimatedGifFile FilePath GifLooping GifDelay deriving (Show, Eq)

data Configuration = Configuration
  { _width  :: Int
  , _height :: Int
  , _output :: Output
  }
  deriving Show
