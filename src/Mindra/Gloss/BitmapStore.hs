module Mindra.Gloss.BitmapStore
  ( clear
  , load
  )
where

import Prelude hiding (lookup)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')

import qualified System.IO.Unsafe as UIO

import qualified Data.HashMap.Strict as HM
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Juicy as GJ

type Store = HM.HashMap String G.Picture

storeIORef = UIO.unsafePerformIO (newIORef (HM.empty :: Store))

clear :: String -> IO G.Picture
clear path = atomicModifyIORef' storeIORef modify where modify kv = (HM.delete path kv, G.Blank)

lookup :: String -> IO (Maybe G.Picture)
lookup path = HM.lookup path <$> readIORef storeIORef

store :: String -> G.Picture -> IO G.Picture
store path value = do
  atomicModifyIORef' storeIORef modify
  where modify kv = (HM.insert path value kv, value)

load :: String -> IO (Maybe G.Picture)
load filePath = do
  existing <- lookup filePath
  case existing of
    Just x  -> return (Just x)
    Nothing -> do
      bmp <- GJ.loadJuicy filePath
      case bmp of
        Just x  -> Just <$> store filePath x
        Nothing -> return Nothing
