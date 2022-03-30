module Mindra.Diagrams where

import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as LT

import Diagrams.Core.Compile (renderDia)
import Graphics.Svg.Core (renderBS)
import Diagrams.TwoD.Size (mkSizeSpec2D)

import Diagrams.Backend.SVG (SVG(..), Options(..))
import Diagrams.Backend.Rasterific (Rasterific(..), Options(..), renderRasterific)

svgToText svg width height = LT.toStrict $ TLE.decodeUtf8 $ renderBS $ renderDia
  SVG
  (SVGOptions dimensions Nothing "" [] True)
  svg
  where dimensions = mkSizeSpec2D (Just (fromIntegral width)) (Just (fromIntegral height))

writeRasterImageToFile raster width height filePath = renderRasterific filePath dimensions raster
  where dimensions = mkSizeSpec2D (Just (fromIntegral width)) (Just (fromIntegral height))
