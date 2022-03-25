import Criterion.Main

import qualified Data.Text as T
import qualified Text.RawString.QQ as QQ

import Mindra.Diagrams.Parser (parseSVG)
import Mindra.Gloss.Parser (parsePicture)

diagramsInput :: T.Text
diagramsInput = [QQ.r|
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
  CenterX FontSize 3 "Large Text"]
|]

glossInput :: T.Text
glossInput = [QQ.r|
[[Translate -200 200 Circle 20
 Translate -150 200 CircleSolid 20
 Translate -100 200 ThickCircle 20 8
 Translate -50 200 Color 0 255 0 255 Circle 20
 Translate 0 200 Color 0 255 0 255 CircleSolid 20
 Translate 50 200 Color 0 255 0 255 ThickCircle 20 8]
[Translate -200 150 RectangleWire 20 20
 Translate -150 150 RectangleSolid 20 20
 Translate -100 150 RectangleUpperSolid 20 20
 Translate -50 150 RectangleUpperWire 20 20
 Translate 0 150 Color 0 0 255 255 RectangleWire 20 20
 Translate 50 150 Color 255 0 0 255 RectangleSolid 20 20]
[Translate -200 100 Arc 0 90 20
 Translate -150 100 ArcSolid 0 90 20
 Translate -100 100 ThickArc 0 90 20 2
 Translate -50 100 SectorWire 30 90 20
 Translate 0 100 Color 0 0 100 100 SectorWire 180 270 20
 Translate 50 100 Color 0 0 100 100 ArcSolid 270 360 20]
[Translate -200 50 Line [[0 0] [0 20]]
 Translate -150 50 Line [[0 0] [10 10] [20 0]]
 Translate -100 50 LineLoop [[0 0] [10 10] [20 0]]
 Translate -50 50 Color 100 100 100 100 Line [[0 0] [0 20]]
 Translate 0 50 Color 100 0 0 100 Line [[0 0] [10 10] [20 0]]
 Translate 50 50 Color 100 100 0 100 Line [[0 0] [20 0]]]
[Translate -200 -50 Polygon [[0 0] [0 40] [40 40] [40 0]]
 Translate -150 -50 Rotate 30 Polygon [[0 0] [0 40] [40 40] [40 0]]
 Translate -50 -50 Color 91 121 100 200
   Polygon [[0 0] [1 1] [2 4] [3 9] [4 16] [5 25] [6 36] [7 9]
            [8 24] [9 1] [10 20] [11 1] [12 24] [13 9] [14 36]
            [15 25] [16 16] [17 9] [18 4] [19 1] [20 0] [21 1]
            [22 4] [23 9] [24 16] [25 25] [26 36] [27 9] [28 24]
            [29 1] [30 20] [31 1] [32 24] [33 9] [34 36] [35 25]
            [36 16] [37 9] [38 4] [39 1] [40 0] [41 1] [42 4] [43 9]
            [44 16] [45 25] [46 36] [47 9] [48 24] [49 1] [50 20]
            [51 1] [52 24] [53 9] [54 36] [55 25] [56 16] [57 9]
            [58 4] [59 1] [60 0] [61 1] [62 4] [63 9] [64 16]
            [65 25] [66 36] [67 9] [68 24] [69 1] [70 20] [71 1]
            [72 24] [73 9] [74 36] [75 25] [76 16] [77 9] [78 4]
            [79 1] [80 0] [81 1] [82 4] [83 9] [84 16] [85 25]
            [86 36] [87 9] [88 24] [89 1] [90 20] [91 1] [92 24]
            [93 9] [94 36] [95 25] [96 16] [97 9] [98 4] [99 1]
            [100 0] [101 1] [102 4] [103 9] [104 16] [105 25] [106 36]
            [107 9] [108 24] [109 1] [110 20] [111 1] [112 24] [113 9]
            [114 36] [115 25] [116 16] [117 9] [118 4] [119 1] [120 0]
            [121 1] [122 4] [123 9] [124 16] [125 25] [126 36] [127 9]
            [128 24] [129 1] [130 20] [131 1] [132 24] [133 9] [134 36]
            [135 25] [136 16] [137 9] [138 4] [139 1] [140 0] [141 1]
            [142 4] [143 9] [144 16] [145 25] [146 36] [147 9] [148 24]
            [149 1] [150 20] [151 1] [152 24] [153 9] [154 36] [155 25]
            [156 16] [157 9] [158 4] [159 1] [160 0] [161 1] [162 4]
            [163 9] [164 16] [165 25] [166 36] [167 9] [168 24] [169 1]
            [170 20] [171 1] [172 24] [173 9] [174 36] [175 25] [176 16]
            [177 9] [178 4] [179 1] [180 0] [181 1] [182 4] [183 9] [184 16]
            [185 25] [186 36] [187 9] [188 24] [189 1] [190 20] [191 1]
            [192 24] [193 9] [194 36] [195 25] [196 16] [197 9] [198 4] [199 1]]]
[Translate -200 -100 Scale 0.125 0.125 "HELLO"
 Translate -100 -100 Scale 0.125 0.125 Color 91 200 100 100 "GLOSS"]]
|]

main :: IO ()
main = do
  defaultMain
    [ bench "parseSVG" $ whnf parseSVG diagramsInput
    , bench "parsePicture" $ whnf parsePicture glossInput
    ]
