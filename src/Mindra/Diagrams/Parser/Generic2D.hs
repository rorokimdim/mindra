module Mindra.Diagrams.Parser.Generic2D where

import Control.Applicative

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (choice, runParser)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Error (ParseErrorBundle)

import qualified Diagrams.Backend.Rasterific as Rasterific
import qualified Diagrams.Backend.SVG as SVG
import qualified Diagrams.Prelude as D
import qualified System.IO.Unsafe as UIO
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Colour (withOpacity)
import Data.Colour.SRGB (sRGB24)
import Diagrams.Combinators (atPoints, beside, cat)
import Diagrams.TwoD (showEnvelope, showOrigin)
import Diagrams.TwoD.Combinators (bg, bgFrame, hcat, hsep, padX, padY, vcat, vsep)
import Diagrams.TwoD.Types (P2, V2, mkP2, mkR2)

import Mindra.Parser.Common
  (Parser, pBool, pDouble, pList, pRGB, pRGBA, pStringLiteral, pWhiteSpace)

pAlignBottom = do
  _ <- string "AlignBottom"
  pWhiteSpace
  d <- pDiagram
  return $ D.alignB d

pAlignLeft = do
  _ <- string "AlignLeft"
  pWhiteSpace
  d <- pDiagram
  return $ D.alignL d

pAlignRight = do
  _ <- string "AlignRight"
  pWhiteSpace
  d <- pDiagram
  return $ D.alignR d

pAlignTop = do
  _ <- string "AlignTop"
  pWhiteSpace
  d <- pDiagram
  return $ D.alignT d

pAlignX = do
  _ <- string "AlignX"
  pWhiteSpace
  n <- pDouble
  pWhiteSpace
  d <- pDiagram
  return $ D.alignX n d

pAlignY = do
  _ <- string "AlignY"
  pWhiteSpace
  n <- pDouble
  pWhiteSpace
  d <- pDiagram
  return $ D.alignY n d

pArrowAt = do
  _ <- string "ArrowAt"
  pWhiteSpace
  p <- pPoint
  pWhiteSpace
  v <- pVector
  optional pWhiteSpace
  opts <- optional pArrowOpts
  return $ case opts of
    Just o  -> D.arrowAt' o p v
    Nothing -> D.arrowAt p v

pArrowBetween = do
  _ <- string "ArrowBetween"
  pWhiteSpace
  p1 <- pPoint
  pWhiteSpace
  p2 <- pPoint
  optional pWhiteSpace
  opts <- optional pArrowOpts
  return $ case opts of
    Just o  -> D.arrowBetween' o p1 p2
    Nothing -> D.arrowBetween p1 p2

pArrowConnect = do
  _ <- string "ArrowConnect"
  pWhiteSpace
  n1 <- pStringLiteral
  pWhiteSpace
  n2 <- pStringLiteral
  pWhiteSpace
  d <- pDiagram
  optional pWhiteSpace
  opts <- optional pArrowOpts
  return $ case opts of
    Just o  -> D.connect' o n1 n2 d
    Nothing -> D.connect n1 n2 d

pArrowConnectOutside = do
  _ <- string "ArrowConnectOutside"
  pWhiteSpace
  n1 <- pStringLiteral
  pWhiteSpace
  n2 <- pStringLiteral
  pWhiteSpace
  d <- pDiagram
  optional pWhiteSpace
  opts <- optional pArrowOpts
  return $ case opts of
    Just o  -> D.connectOutside' o n1 n2 d
    Nothing -> D.connectOutside n1 n2 d

pArrowConnectPerim = do
  _ <- string "ArrowConnectPerim"
  pWhiteSpace
  n1 <- pStringLiteral
  pWhiteSpace
  n2 <- pStringLiteral
  pWhiteSpace
  a1 <- pDouble
  pWhiteSpace
  a2 <- pDouble
  pWhiteSpace
  d <- pDiagram
  optional pWhiteSpace
  opts <- optional pArrowOpts
  return $ case opts of
    Just o  -> D.connectPerim' o n1 n2 (a1 D.@@ D.deg) (a2 D.@@ D.deg) d
    Nothing -> D.connectPerim n1 n2 (a1 D.@@ D.deg) (a2 D.@@ D.deg) d

pArrowHead :: Parser (D.ArrowHT Double)
pArrowHead = do
  arrowHead <- choice
    [ string "tri"
    , string "dart"
    , string "halfDart"
    , string "spike"
    , string "thorn"
    , string "lineHead"
    , string "noHead"
    ]
  return $ f arrowHead
 where
  f "tri"      = D.tri
  f "dart"     = D.dart
  f "halfDart" = D.halfDart
  f "spike"    = D.spike
  f "thorn"    = D.thorn
  f "lineHead" = D.lineHead
  f "noHead"   = D.noHead
  f _          = D.dart

pArrowTail :: Parser (D.ArrowHT Double)
pArrowTail = do
  arrowTail <- choice
    [ string "tri"
    , string "dart"
    , string "halfDart"
    , string "spike"
    , string "thorn"
    , string "lineTail"
    , string "noTail"
    , string "quill"
    , string "block"
    ]
  return $ f arrowTail
 where
  f "tri"      = D.tri'
  f "dart"     = D.dart'
  f "halfDart" = D.halfDart'
  f "spike"    = D.spike'
  f "thorn"    = D.thorn'
  f "lineTail" = D.lineTail
  f "noTail"   = D.noTail
  f "quill"    = D.quill
  f "block"    = D.block
  f _          = D.dart

pArrowOpts :: Parser (D.ArrowOpts Double)
pArrowOpts = do
  _ <- string "ArrowOpts"
  pWhiteSpace
  arrowHead <- pArrowHead
  pWhiteSpace
  arrowTail <- pArrowTail
  pWhiteSpace
  headGap <- pDouble
  pWhiteSpace
  tailGap <- pDouble
  pWhiteSpace
  (hr, hg, hb, ha) <- pRGBA
  pWhiteSpace
  (tr, tg, tb, ta) <- pRGBA
  optional pWhiteSpace

  shaftPoints <- optional $ pList pPoint
  let
    arrowShaft = case shaftPoints of
      Just [] -> D.straightShaft
      Just ps -> D.cubicSpline False ps
      Nothing -> D.straightShaft

  let hc = sRGB24 (fromIntegral hr) (fromIntegral hg) (fromIntegral hb)
  let tc = sRGB24 (fromIntegral tr) (fromIntegral tg) (fromIntegral tb)

  return
    (    D.with
    D.&  D.arrowHead
    D..~ arrowHead
    D.&  D.arrowTail
    D..~ arrowTail
    D.&  D.arrowShaft
    D..~ arrowShaft
    D.&  D.headGap
    D..~ D.global headGap
    D.&  D.tailGap
    D..~ D.global tailGap
    D.&  D.headStyle
    D.%~ (D.fc hc . D.opacity (fromIntegral ha / 255))
    D.&  D.tailStyle
    D.%~ (D.fc tc . D.opacity (fromIntegral ta / 255))
    )

pBackground = do
  _ <- string "Background"
  pWhiteSpace
  (r, g, b) <- pRGB
  pWhiteSpace
  d <- pDiagram
  let c = sRGB24 (fromIntegral r) (fromIntegral g) (fromIntegral b)
  return $ bg c d

pBackgroundFrame = do
  _ <- string "BackgroundFrame"
  pWhiteSpace
  p <- pDouble
  pWhiteSpace
  (r, g, b) <- pRGB
  pWhiteSpace
  d <- pDiagram
  let c = sRGB24 (fromIntegral r) (fromIntegral g) (fromIntegral b)
  return $ bgFrame p c d

pBeside = do
  _ <- string "Beside"
  pWhiteSpace
  x <- pDouble
  pWhiteSpace
  y <- pDouble
  pWhiteSpace
  d1 <- pDiagram
  d2 <- pDiagram
  return $ beside (mkR2 x y) d1 d2

pBSpline = do
  _ <- string "BSpline"
  pWhiteSpace
  ps <- pList pPoint
  return $ D.bspline ps

pCat = do
  _ <- string "Cat"
  pWhiteSpace
  x <- pDouble
  pWhiteSpace
  y <- pDouble
  pWhiteSpace
  ds <- pList pDiagram
  return $ cat (mkR2 x y) ds

pCenterX = do
  _ <- string "CenterX"
  pWhiteSpace
  d <- pDiagram
  return $ D.centerX d

pCenterXY = do
  _ <- string "CenterXY"
  pWhiteSpace
  d <- pDiagram
  return $ D.centerXY d

pCenterY = do
  _ <- string "CenterY"
  pWhiteSpace
  d <- pDiagram
  return $ D.centerY d

pCircle = do
  _ <- string "Circle"
  pWhiteSpace
  radius <- pDouble
  return $ D.circle radius

pCubicSpline = do
  _ <- string "CubicSpline"
  pWhiteSpace
  closed <- pBool
  pWhiteSpace
  ps <- pList pPoint
  return $ D.cubicSpline closed ps

pDashingG = do
  _ <- string "DashingG"
  optional pWhiteSpace
  ns <- pList pDouble
  pWhiteSpace
  n <- L.decimal
  pWhiteSpace
  d <- pDiagram
  return $ D.dashingG ns n d

pDashingL = do
  _ <- string "DashingL"
  pWhiteSpace
  ns <- pList pDouble
  pWhiteSpace
  n <- L.decimal
  pWhiteSpace
  d <- pDiagram
  return $ D.dashingL ns n d

pDashingN = do
  _ <- string "DashingN"
  pWhiteSpace
  ns <- pList pDouble
  pWhiteSpace
  n <- L.decimal
  pWhiteSpace
  d <- pDiagram
  return $ D.dashingN ns n d

pEllipse = do
  _ <- string "Ellipse"
  pWhiteSpace
  eccentricity <- pDouble
  if eccentricity < 0 || eccentricity >= 1
    then fail $ "eccentricity of an ellipse must be >= 0 and < 1; got " <> show eccentricity
    else return $ D.ellipse eccentricity

pEllipseXY = do
  _ <- string "EllipseXY"
  pWhiteSpace
  radiusX <- pDouble
  pWhiteSpace
  radiusY <- pDouble
  return $ D.ellipseXY radiusX radiusY

pFillColor = do
  _ <- choice [string "FillColor", string "FillColour"]
  pWhiteSpace
  (r, g, b, a) <- pRGBA
  pWhiteSpace
  d <- pDiagram
  let c  = sRGB24 (fromIntegral r) (fromIntegral g) (fromIntegral b)
  let ca = withOpacity c $ fromIntegral a / 255
  return $ D.fillColor ca d

pFillOpacity = do
  _ <- string "FillOpacity"
  pWhiteSpace
  a <- pDouble
  pWhiteSpace
  d <- pDiagram
  return $ D.fillOpacity a d

pFontSize = do
  _ <- string "FontSize"
  pWhiteSpace
  s <- pDouble
  pWhiteSpace
  d <- pDiagram
  return $ D.fontSizeL s d

pHRule = do
  _ <- string "HRule"
  pWhiteSpace
  length <- pDouble
  return $ D.hrule length

pHCat = do
  _ <- string "HCat"
  optional pWhiteSpace
  ds <- pList pDiagram
  return $ hcat ds

pHSep = do
  _ <- string "HSep"
  pWhiteSpace
  sep <- pDouble
  pWhiteSpace
  ds <- pList pDiagram
  return $ hsep sep ds

pImage = do
  _ <- string "Image"
  pWhiteSpace
  filePath <- pStringLiteral
  pWhiteSpace
  width <- pDouble
  pWhiteSpace
  height <- pDouble
  let image = UIO.unsafePerformIO $ D.loadImageEmb filePath
  case image of
    Left  estr -> fail estr
    Right img  -> return $ D.sized (D.dims2D width height) (D.image img)

pLine = do
  _ <- string "Line"
  optional pWhiteSpace
  vs <- pList pPoint
  return $ D.fromVertices vs

pLineCloseLoop = do
  _ <- string "LineCloseLoop"
  optional pWhiteSpace
  vs <- pList pPoint
  return $ D.strokeLoop $ D.closeLine $ D.fromVertices vs

pLineColor = do
  _ <- choice [string "LineColor", string "LineColour"]
  pWhiteSpace
  (r, g, b, a) <- pRGBA
  pWhiteSpace
  d <- pDiagram
  let c  = sRGB24 (fromIntegral r) (fromIntegral g) (fromIntegral b)
  let ca = withOpacity c $ fromIntegral a / 255
  return $ D.lineColor ca d

pLineGlueLoop = do
  _ <- string "LineGlueLoop"
  optional pWhiteSpace
  vs <- pList pPoint
  return $ D.strokeLoop $ D.glueLine $ D.fromVertices vs

pLineWidthG = do
  _ <- string "LineWidthG"
  pWhiteSpace
  w <- pDouble
  pWhiteSpace
  d <- pDiagram
  return $ D.lwG w d

pLineWidthL = do
  _ <- string "LineWidthL"
  pWhiteSpace
  w <- pDouble
  pWhiteSpace
  d <- pDiagram
  return $ D.lwL w d

pLineWidthN = do
  _ <- string "LineWidthN"
  pWhiteSpace
  w <- pDouble
  pWhiteSpace
  d <- pDiagram
  return $ D.lwN w d

pNamed = do
  _ <- string "Named"
  pWhiteSpace
  name <- pStringLiteral
  pWhiteSpace
  d <- pDiagram
  return $ D.named name d

pPad = do
  _ <- string "Pad"
  pWhiteSpace
  px <- pDouble
  pWhiteSpace
  py <- pDouble
  pWhiteSpace
  d <- pDiagram
  return $ padY py $ padX px d

pPoint :: Parser (P2 Double)
pPoint = do
  _ <- string "["
  optional pWhiteSpace
  f1 <- pDouble
  pWhiteSpace
  f2 <- pDouble
  optional pWhiteSpace
  _ <- string "]"
  return $ mkP2 f1 f2

pPolygonRegular = do
  _ <- string "PolygonRegular"
  pWhiteSpace
  n <- L.decimal
  pWhiteSpace
  length <- pDouble
  return $ D.regPoly n length

pPolygonSides = do
  _ <- string "PolygonSides"
  pWhiteSpace
  angles <- pList pDouble
  optional pWhiteSpace
  lengths <- pList pDouble
  return $ D.polygon (D.with D.& D.polyType D..~ D.PolySides (map (D.@@ D.deg) angles) lengths)

pPolygonPolar = do
  _ <- string "PolygonPolar"
  pWhiteSpace
  angles <- pList pDouble
  optional pWhiteSpace
  radii <- pList pDouble
  return $ D.polygon (D.with D.& D.polyType D..~ D.PolyPolar (map (D.@@ D.deg) angles) radii)

pPosition = do
  _ <- string "Position"
  optional pWhiteSpace
  ps <- pList pPoint
  optional pWhiteSpace
  ds <- pList pDiagram
  return $ atPoints ps ds

pRectangle = do
  _ <- string "Rectangle"
  pWhiteSpace
  width <- pDouble
  pWhiteSpace
  height <- pDouble
  return $ D.rect width height

pReflectX = do
  _ <- string "ReflectX"
  pWhiteSpace
  d <- pDiagram
  return $ D.reflectX d

pReflectY = do
  _ <- string "ReflectY"
  pWhiteSpace
  d <- pDiagram
  return $ D.reflectY d

pStrokeLine = do
  _ <- string "StrokeLine"
  optional pWhiteSpace
  vs <- pList pPoint
  let line = D.fromVertices vs
  return $ D.strokeLine line

pStrokeOpacity = do
  _ <- string "StrokeOpacity"
  pWhiteSpace
  a <- pDouble
  pWhiteSpace
  d <- pDiagram
  return $ D.strokeOpacity a d

pVector :: Parser (V2 Double)
pVector = do
  _ <- string "["
  optional pWhiteSpace
  f1 <- pDouble
  pWhiteSpace
  f2 <- pDouble
  optional pWhiteSpace
  _ <- string "]"
  return $ mkR2 f1 f2

pVCat = do
  _ <- string "VCat"
  optional pWhiteSpace
  ds <- pList pDiagram
  return $ vcat ds

pVSep = do
  _ <- string "VSep"
  pWhiteSpace
  sep <- pDouble
  pWhiteSpace
  ds <- pList pDiagram
  return $ vsep sep ds

pRotate = do
  _ <- string "Rotate"
  pWhiteSpace
  degrees <- pDouble
  pWhiteSpace
  d <- pDiagram
  return $ D.rotate (degrees D.@@ D.deg) d

pRotateBy = do
  _ <- string "RotateBy"
  pWhiteSpace
  f <- pDouble
  pWhiteSpace
  d <- pDiagram
  return $ D.rotateBy f d

pRoundedRectangle = do
  _ <- string "RoundedRectangle"
  pWhiteSpace
  width <- pDouble
  pWhiteSpace
  height <- pDouble
  pWhiteSpace
  radius <- pDouble
  return $ D.roundedRect width height radius

pScale = do
  _ <- string "Scale"
  pWhiteSpace
  sx <- pDouble
  pWhiteSpace
  sy <- pDouble
  pWhiteSpace
  d <- pDiagram
  return $ D.scaleY sy $ D.scaleX sx d

pShowEnvelope = do
  _ <- string "ShowEnvelope"
  pWhiteSpace
  d <- pDiagram
  return $ showEnvelope d

pShowOrigin = do
  _ <- string "ShowOrigin"
  pWhiteSpace
  d <- pDiagram
  return $ showOrigin d

pSuperimpose = do
  optional $ string "Superimpose"
  optional pWhiteSpace
  ds <- pList pDiagram
  return $ mconcat ds

pSquare = do
  _ <- string "Square"
  pWhiteSpace
  length <- pDouble
  return $ D.square length

pText = do
  s <- pStringLiteral
  return $ D.text s

pTranslate = do
  _ <- string "Translate"
  pWhiteSpace
  x <- pDouble
  pWhiteSpace
  y <- pDouble
  pWhiteSpace
  d <- pDiagram
  return $ D.translate (mkR2 x y) d

pTriangle = do
  _ <- string "Triangle"
  pWhiteSpace
  length <- pDouble
  return $ D.triangle length

pUnitCircle = do
  _ <- string "UnitCircle"
  return D.unitCircle

pUnitSquare = do
  _ <- string "UnitSquare"
  return D.unitSquare

pVRule = do
  _ <- string "VRule"
  pWhiteSpace
  length <- pDouble
  return $ D.vrule length

pDiagram = do
  optional pWhiteSpace
  p <- choice
    [ pAlignBottom
    , pAlignLeft
    , pAlignRight
    , pAlignTop
    , pAlignX
    , pAlignY
    , pArrowConnectOutside
    , pArrowConnectPerim
    , pArrowConnect
    , pArrowAt
    , pArrowBetween
    , pBackgroundFrame
    , pBackground
    , pBeside
    , pBSpline
    , pCat
    , pCenterXY
    , pCenterX
    , pCenterY
    , pCircle
    , pCubicSpline
    , pDashingG
    , pDashingL
    , pDashingN
    , pEllipseXY
    , pEllipse
    , pFillColor
    , pFontSize
    , pHCat
    , pHRule
    , pHSep
    , pImage
    , pLineCloseLoop
    , pLineColor
    , pLineGlueLoop
    , pLineWidthG
    , pLineWidthL
    , pLineWidthN
    , pLine
    , pNamed
    , pPad
    , pPolygonPolar
    , pPolygonRegular
    , pPolygonSides
    , pPosition
    , pShowEnvelope
    , pShowOrigin
    , pSquare
    , pRectangle
    , pReflectX
    , pReflectY
    , pRotateBy
    , pRotate
    , pRoundedRectangle
    , pScale
    , pSuperimpose
    , pStrokeOpacity
    , pText
    , pTranslate
    , pTriangle
    , pUnitCircle
    , pUnitSquare
    , pVCat
    , pVRule
    , pVSep
    ]
  optional pWhiteSpace
  return p

pAnimatedGif = do
  optional pWhiteSpace
  pList pDiagram

parseSVG :: Text -> Either (ParseErrorBundle Text Void) (D.Diagram SVG.B)
parseSVG = runParser pDiagram "source"

parseRasterific :: Text -> Either (ParseErrorBundle Text Void) (D.Diagram Rasterific.B)
parseRasterific = runParser pDiagram "source"

parseAnimatedGif :: Text -> Either (ParseErrorBundle Text Void) [D.Diagram Rasterific.B]
parseAnimatedGif = runParser pAnimatedGif "source"
