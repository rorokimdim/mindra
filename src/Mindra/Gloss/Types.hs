module Mindra.Gloss.Types where

import qualified Graphics.Gloss as G

data Mode = Static | Interactive deriving (Eq, Show)

data Configuration = Configuration { _windowWidth :: Int
                                   , _windowHeight :: Int
                                   , _windowTitle :: String
                                   , _windowX:: Int
                                   , _windowY:: Int
                                   , _backgroundColor :: G.Color
                                   , _stepsPerSecond :: Int
                                   , _mode :: Mode
                                   , _noEvent :: Bool
                                   , _noStep :: Bool
                                   } deriving (Show)
