module Main where

import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (isEOF, hFlush, stdout)
import Text.Megaparsec (parseTest)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Read (readMaybe)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Data.Picture as G
import qualified Graphics.Gloss.Interface.IO.Game as G
import qualified Graphics.Gloss.Interface.IO.Simulate as G

import qualified Mindra.Diagrams as MD
import qualified Mindra.Diagrams.Parser as MDP
import qualified Mindra.Diagrams.Types as MDT (Configuration(..), Output(..))
import qualified Mindra.Gloss.Parser as MGP
import qualified Mindra.Gloss.Types as MGT (Configuration(..), Mode(..))
import qualified Version

import Mindra.Parser.Configuration (MindraConfiguration(..), parseConfiguration)

type World = T.Text
type Seconds = Float
type Message = T.Text
type MessageTag = T.Text
type MessageBody = T.Text

exitOnShutdown :: MessageTag -> IO ()
exitOnShutdown "SHUTDOWN" = do
  writeAndReadMessageForTag "SHUTDOWN" T.empty "OK"
  exitSuccess
exitOnShutdown _ = return ()

toText :: Show a => a -> T.Text
toText n = T.pack $ show n

keyToText :: G.Key -> T.Text
keyToText (G.SpecialKey  k                     ) = ":special" <> toText k
keyToText (G.Char        k                     ) = ":" <> toText k
keyToText (G.MouseButton (G.AdditionalButton n)) = ":mouseAdditional" <> toText n
keyToText (G.MouseButton b                     ) = ":mouse" <> toText b

handleEvent :: G.Event -> World -> IO World
handleEvent (G.EventKey key keyState (G.Modifiers shiftState ctrlState altState) (x, y)) w = do
  writeAndReadMessageForTag
    "EVENT"
    (  "EventKey "
    <> keyToText key
    <> " "
    <> ":"
    <> T.toLower (toText keyState)
    <> " "
    <> " :shift"
    <> toText shiftState
    <> " :ctrl"
    <> toText ctrlState
    <> " :alt"
    <> toText altState
    <> " "
    <> toText x
    <> " "
    <> toText y
    )
    "OK"
handleEvent (G.EventMotion (x, y)) w = do
  writeAndReadMessageForTag "EVENT" ("EventMotion " <> toText x <> " " <> toText y) "OK"
handleEvent (G.EventResize (x, y)) w = do
  writeAndReadMessageForTag "EVENT" ("EventResize " <> toText x <> " " <> toText y) "OK"

handleEventNoop :: G.Event -> World -> IO World
handleEventNoop _ w = return w

handleStep :: Seconds -> World -> IO World
handleStep s w = do
  writeAndReadMessageForTag "STEP " (toText s) "OK"

handleStepNoop :: Seconds -> World -> IO World
handleStepNoop _ w = return w

worldToPicture :: World -> IO G.Picture
worldToPicture w = do
  body <- writeAndReadMessageForTag "READY" "PICTURE" "PICTURE"
  case MGP.parsePicture body of
    Left err -> do
      writeMessage "FAIL" $ T.pack (errorBundlePretty err)
      worldToPicture w
    Right p -> return p

run :: MindraConfiguration -> IO ()
run (GlossConfiguration MGT.Configuration {..}) = do
  let
    display = if _fullScreen
      then G.FullScreen
      else G.InWindow _windowTitle (_windowWidth, _windowHeight) (_windowX, _windowY)
  if _mode == MGT.Interactive && _noEvent
    then G.simulateIO
      display
      _backgroundColor
      _stepsPerSecond
      T.empty
      worldToPicture
      (const (if _noStep then handleStepNoop else handleStep))
    else if _mode == MGT.Interactive
      then G.playIO
        display
        _backgroundColor
        _stepsPerSecond
        T.empty
        worldToPicture
        (if _noEvent then handleEventNoop else handleEvent)
        (if _noStep then handleStepNoop else handleStep)
      else do
        body <- writeAndReadMessageForTag "READY" "PICTURE" "PICTURE"
        case MGP.parsePicture body of
          Left err -> do
            writeMessage "FAIL" $ T.pack (errorBundlePretty err)
            main
          Right p -> G.display display _backgroundColor p
run (DiagramsConfiguration MDT.Configuration { _output = MDT.SVGText, ..}) = do
  body <- writeAndReadMessageForTag "READY" "SVG" "SVG"
  case MDP.parseSVG body of
    Left err -> do
      writeMessage "FAIL" $ T.pack (errorBundlePretty err)
      main
    Right svg -> writeMessage "SVG" (MD.svgToText svg _width _height)
run (DiagramsConfiguration MDT.Configuration { _output = MDT.SVGFile filePath, ..}) = do
  body <- writeAndReadMessageForTag "READY" "SVG" "SVG"
  case MDP.parseSVG body of
    Left err -> do
      writeMessage "FAIL" $ T.pack (errorBundlePretty err)
      main
    Right svg -> do
      let svgText = MD.svgToText svg _width _height
      TIO.writeFile filePath svgText
      writeMessage "SVG" svgText
run (DiagramsConfiguration MDT.Configuration { _output = MDT.RasterFile filePath, ..}) = do
  body <- writeAndReadMessageForTag "READY" "RASTER" "RASTER"
  case MDP.parseRasterific body of
    Left err -> do
      writeMessage "FAIL" $ T.pack (errorBundlePretty err)
      main
    Right raster -> do
      MD.writeRasterImageToFile raster _width _height filePath
      writeMessage "RASTER" (T.pack filePath)

isComment :: T.Text -> Bool
isComment t = T.isPrefixOf "#" t || T.isPrefixOf "--" t || T.isPrefixOf ";" t

readUntilBlankLine :: IO T.Text
readUntilBlankLine = readUntilBlankLine' ""
 where
  readUntilBlankLine' body = do
    end <- isEOF
    if end
      then return body
      else do
        line <- TIO.getLine
        let input = T.strip line
        if input == T.empty
          then return body
          else if isComment input
            then readUntilBlankLine' body
            else do
              newBody <- readUntilBlankLine' $ body <> "\n" <> input
              return $ T.strip newBody

wait :: MessageBody -> IO ()
wait body = do
  let microseconds = readMaybe (T.unpack body) :: Maybe Int
  threadDelay $ fromMaybe 1000000 microseconds

readMessage :: IO (MessageTag, MessageBody)
readMessage = do
  input <- readUntilBlankLine
  if input == T.empty
    then readMessage
    else do
      let ws         = T.words input
      let (tag : xs) = ws
      let body       = T.unwords xs
      if tag == "WAIT"
        then do
          wait body
          readMessage
        else do
          exitOnShutdown tag
          return (tag, body)

writeMessage :: MessageTag -> MessageBody -> IO ()
writeMessage tag body | body == T.empty = do
  TIO.putStrLn tag
  hFlush stdout
writeMessage tag body = do
  TIO.putStrLn $ tag <> " " <> T.intercalate "\\n" (T.lines body)
  hFlush stdout

writeAndReadMessage :: MessageTag -> MessageBody -> IO (MessageTag, MessageBody)
writeAndReadMessage tag body = do
  writeMessage tag body
  readMessage

writeAndReadMessageForTag :: MessageTag -> MessageTag -> MessageTag -> IO MessageBody
writeAndReadMessageForTag tag body expectedTag = do
  (t, b) <- writeAndReadMessage tag body
  if t == expectedTag
    then return b
    else do
      writeMessage "FAIL" ("Expected a message with tag '" <> expectedTag <> "'; got " <> t)
      writeAndReadMessageForTag tag body expectedTag

main :: IO ()
main = do
  args <- getArgs
  process args
 where
  process [] = do
    body <- writeAndReadMessageForTag "READY" "INIT" "INIT"
    case parseConfiguration body of
      Left err -> do
        writeMessage "FAIL" err
        main
      Right cfg -> run cfg
  process xs | head xs == "-v" || head xs == "--version" = putStrLn Version.appVersion
