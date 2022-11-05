module Mindra.Diagrams where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TLE

import Diagrams.Core.Compile (renderDia)
import Diagrams.TwoD.Size (mkSizeSpec2D)
import Graphics.Svg.Core (renderBS)

import Diagrams.Backend.Rasterific (animatedGif, renderRasterific)
import Diagrams.Backend.SVG (Options(..), SVG(..))

svgToText svg width height = LT.toStrict $ TLE.decodeUtf8 $ renderBS $ renderDia
  SVG
  (SVGOptions dimensions Nothing "" [] True)
  svg
  where dimensions = mkSizeSpec2D (Just (fromIntegral width)) (Just (fromIntegral height))

writeRasterImageToFile raster width height filePath = renderRasterific filePath dimensions raster
  where dimensions = mkSizeSpec2D (Just (fromIntegral width)) (Just (fromIntegral height))

writeAnimatedGifToFile rasters width height looping delay filePath = animatedGif
  filePath
  dimensions
  looping
  delay
  rasters
  where dimensions = mkSizeSpec2D (Just (fromIntegral width)) (Just (fromIntegral height))
