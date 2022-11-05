module Mindra.Gloss.Parser.Picture where

import Control.Applicative

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (choice, runParser)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Error (ParseErrorBundle)

import qualified Graphics.Gloss as G
import qualified System.IO.Unsafe as UIO
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Mindra.Gloss.BitmapStore as BitmapStore

import Mindra.Parser.Common (Parser, pFloat, pList, pRGBA, pStringLiteral, pWhiteSpace)

pBlank :: Parser G.Picture
pBlank = G.Blank <$ string "Blank"

pCircle :: Parser G.Picture
pCircle = do
  _ <- string "Circle"
  pWhiteSpace
  f1 <- pFloat
  return $ G.Circle f1

pCircleSolid :: Parser G.Picture
pCircleSolid = do
  _ <- string "CircleSolid"
  pWhiteSpace
  f1 <- pFloat
  return $ G.circleSolid f1

pThickCircle :: Parser G.Picture
pThickCircle = do
  _ <- string "ThickCircle"
  pWhiteSpace
  f1 <- pFloat
  pWhiteSpace
  f2 <- pFloat
  return $ G.ThickCircle f1 f2

pArc :: Parser G.Picture
pArc = do
  _ <- string "Arc"
  pWhiteSpace
  f1 <- pFloat
  pWhiteSpace
  f2 <- pFloat
  pWhiteSpace
  f3 <- pFloat
  return $ G.Arc f1 f2 f3

pArcSolid :: Parser G.Picture
pArcSolid = do
  _ <- string "ArcSolid"
  pWhiteSpace
  f1 <- pFloat
  pWhiteSpace
  f2 <- pFloat
  pWhiteSpace
  f3 <- pFloat
  return $ G.arcSolid f1 f2 f3

pThickArc :: Parser G.Picture
pThickArc = do
  _ <- string "ThickArc"
  pWhiteSpace
  f1 <- pFloat
  pWhiteSpace
  f2 <- pFloat
  pWhiteSpace
  f3 <- pFloat
  pWhiteSpace
  f4 <- pFloat
  return $ G.ThickArc f1 f2 f3 f4

pRectangleSolid :: Parser G.Picture
pRectangleSolid = do
  _ <- string "RectangleSolid"
  pWhiteSpace
  f1 <- pFloat
  pWhiteSpace
  f2 <- pFloat
  return $ G.rectangleSolid f1 f2

pRectangleUpperWire :: Parser G.Picture
pRectangleUpperWire = do
  _ <- string "RectangleUpperWire"
  pWhiteSpace
  f1 <- pFloat
  pWhiteSpace
  f2 <- pFloat
  return $ G.rectangleUpperWire f1 f2

pRectangleUpperSolid :: Parser G.Picture
pRectangleUpperSolid = do
  _ <- string "RectangleUpperSolid"
  pWhiteSpace
  f1 <- pFloat
  pWhiteSpace
  f2 <- pFloat
  return $ G.rectangleUpperSolid f1 f2

pRectangleWire :: Parser G.Picture
pRectangleWire = do
  _ <- string "RectangleWire"
  pWhiteSpace
  f1 <- pFloat
  pWhiteSpace
  f2 <- pFloat
  return $ G.rectangleWire f1 f2

pSectorWire :: Parser G.Picture
pSectorWire = do
  _ <- string "SectorWire"
  pWhiteSpace
  f1 <- pFloat
  pWhiteSpace
  f2 <- pFloat
  pWhiteSpace
  f3 <- pFloat
  return $ G.sectorWire f1 f2 f3

pPoint :: Parser G.Point
pPoint = do
  _ <- string "["
  optional pWhiteSpace
  f1 <- pFloat
  pWhiteSpace
  f2 <- pFloat
  optional pWhiteSpace
  _ <- string "]"
  optional pWhiteSpace
  return (f1, f2)

pPath :: Parser G.Path
pPath = pList pPoint

pLine :: Parser G.Picture
pLine = do
  _ <- string "Line"
  optional pWhiteSpace
  path <- pPath
  return $ G.Line path

pLineLoop :: Parser G.Picture
pLineLoop = do
  _ <- string "LineLoop"
  pWhiteSpace
  path <- pPath
  return $ G.lineLoop path

pPolygon :: Parser G.Picture
pPolygon = do
  _ <- string "Polygon"
  pWhiteSpace
  path <- pPath
  return $ G.Polygon path

pText :: Parser G.Picture
pText = do
  s <- pStringLiteral
  return $ G.Text s

pImage :: Parser G.Picture
pImage = do
  _ <- string "Image"
  pWhiteSpace
  filePath <- pStringLiteral
  let bmp = UIO.unsafePerformIO $ BitmapStore.load filePath
  case bmp of
    Just x  -> return x
    Nothing -> fail ("Invalid image file path " ++ filePath)

pImageSection :: Parser G.Picture
pImageSection = do
  _ <- string "ImageSection"
  pWhiteSpace
  x <- L.decimal
  pWhiteSpace
  y <- L.decimal
  pWhiteSpace
  width <- L.decimal
  pWhiteSpace
  height <- L.decimal
  pWhiteSpace
  (G.Bitmap bitmapData) <- pImage
  return $ G.BitmapSection (G.Rectangle (x, y) (width, height)) bitmapData

pImageClear :: Parser G.Picture
pImageClear = do
  _ <- string "ImageClear"
  pWhiteSpace
  filePath <- pStringLiteral
  return $ UIO.unsafePerformIO $ BitmapStore.clear filePath

pRotate :: Parser G.Picture
pRotate = do
  _ <- string "Rotate"
  pWhiteSpace
  degrees <- pFloat
  pWhiteSpace
  p <- pPicture
  return $ G.Rotate degrees p

pScale :: Parser G.Picture
pScale = do
  _ <- string "Scale"
  pWhiteSpace
  f1 <- pFloat
  pWhiteSpace
  f2 <- pFloat
  pWhiteSpace
  p <- pPicture
  return $ G.Scale f1 f2 p

pTranslate :: Parser G.Picture
pTranslate = do
  _ <- string "Translate"
  pWhiteSpace
  f1 <- pFloat
  pWhiteSpace
  f2 <- pFloat
  pWhiteSpace
  p <- pPicture
  return $ G.Translate f1 f2 p

pPictures :: Parser G.Picture
pPictures = do
  optional $ string "Pictures"
  optional pWhiteSpace
  ps <- pList pPicture
  return $ G.Pictures ps

pColor :: Parser G.Picture
pColor = do
  _ <- string "Color"
  pWhiteSpace
  (r, g, b, a) <- pRGBA
  pWhiteSpace
  p <- pPicture
  return $ G.Color (G.makeColorI r g b a) p

pPicture :: Parser G.Picture
pPicture = do
  optional pWhiteSpace
  p <- choice
    [ pBlank
    , pPolygon
    , pLineLoop
    , pLine
    , pCircleSolid
    , pCircle
    , pImageSection
    , pImageClear
    , pImage
    , pThickCircle
    , pArcSolid
    , pArc
    , pThickArc
    , pText
    , pPictures
    , pColor
    , pRectangleSolid
    , pRectangleUpperSolid
    , pRectangleUpperWire
    , pRectangleWire
    , pSectorWire
    , pRotate
    , pScale
    , pTranslate
    ]
  optional pWhiteSpace
  return p

parse :: Text -> Either (ParseErrorBundle Text Void) G.Picture
parse = runParser pPicture "source"
