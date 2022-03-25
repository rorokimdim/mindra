module Mindra.Diagrams.ParserTest where

import Data.Either (isLeft, isRight)

import qualified Text.RawString.QQ as QQ

import Mindra.Diagrams.Parser (parseConfiguration, parseSVG)
import Mindra.Diagrams (svgToText)

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Diagrams Parser Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup
  "Unit Tests for Diagrams Parser"
  [ testCase "parseConfiguration" $ do
    assertBool "Valid case" $ isRight $ parseConfiguration "Diagrams SVG 200 300"
    assertBool "Valid case with spaces" $ isRight $ parseConfiguration
      " Diagrams    SVG   200   300  "
    assertBool "Valid case with newlines, spaces, tabs" $ isRight $ parseConfiguration
      "Diagrams\n    SVG \n  200 \n  \t 300  "
    assertBool "Invalid case with non-numeric width" $ isLeft $ parseConfiguration
      "Diagrams SVG abc 200"
    assertBool "Invalid case with non-numeric height" $ isLeft $ parseConfiguration
      "Diagrams SVG 200 abc"
  , testCase "parseSVG" $ do
    assertBool "Line" $ isRight $ parseSVG "Line [[0 0] [10 10]]"
    assertBool "LineCloseLoop" $ isRight $ parseSVG "LineCloseLoop [[0 0] [10 10]]"
    assertBool "LineColor" $ isRight $ parseSVG
      "LineColor 200 0 0 255 LineCloseLoop [[0 0] [10 10]]"
    assertBool "HSep" $ isRight $ parseSVG
      "HSep 2 [Circle 10 Circle 20 LineCloseLoop [[0 0] [10 10]]]"
    assertBool "VSep" $ isRight $ parseSVG
      "VSep 2 [Circle 10 Circle 20 LineCloseLoop [[0 0] [10 10]]]"
    assertBool "Big Test" $ isRight $ parseSVG [QQ.r|
               [ Line [[0 0] [1 1] [0 1] [2 0]]
                 Circle 100
                 Circle 200
                 LineCloseLoop [[0 0] [1 2] [4 5]]
                 HSep 2 [Circle 10 Circle 20]
                 VSep 1 []
                 HSep 1 []
                 [Circle 10 Circle 20]
               ]
               |]
    assertBool "Big Test -- draw-basic" $ isRight $ parseSVG [QQ.r|
               Pad 1.25 1 CenterXY VSep 2 [
                 CenterX VSep 2 [
                   HSep 2 ["Circles" HSep 1 [Circle 1 Circle 1.5 Circle 2.0 Circle 2.5]]
                   HSep 2 ["Squares" HSep 1 [Rectangle 1 1 Rectangle 1.5 1.5
                                             Rectangle 2.0 2.0 Rectangle 2.5 2.5]]
                                     HSep 2 ["Lines" VSep 1
                                       [HSep 1 [HRule 1 HRule 1.25 HRule 1.5 HRule 1.75
                                                HRule 2.0 HRule 2.25 HRule 2.5 HRule 2.75]
                                        HSep 1 [VRule 1 VRule 1.25 VRule 1.5 VRule 1.75
                                                VRule 2.0 VRule 2.25 VRule 2.5 VRule 2.75]
                                       ]
                                    ]
                                    HSep 2 ["Ellipses"
                                            HSep 1 [Ellipse 0.0 Ellipse 0.2 Ellipse 0.4
                                                    Ellipse 0.6000000000000001 Ellipse 0.8]
                                           ]
                 ]
                 CenterX FontSize 3 "Large Text"]|]
  ]
