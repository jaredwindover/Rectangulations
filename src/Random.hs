module Random
    ( makeRandomColors,
      getRandomRect,
      getRandomLine,
      getRandomMultiSegmentLine,
      getRandomMultiSegmentRectilinearLine,
      getRandomPoint,
      getRandomElem
    ) where
import Prelude hiding(repeat)

import Data.Word (Word8)
import Data.List (intercalate)
import Graphics.X11.Xlib (Display, Pixel, Dimension)
import Numeric
import System.Random
import Control.Monad (replicateM)
import Control.Monad.State.Lazy(MonadIO, liftIO)

import Color (initColor)
import String (padLeft)
import Draw (Graphical, Coord, rectangle, line, point)

getRandomElem :: MonadIO m => [t] -> m t
getRandomElem xs = do
  let l = length xs
  i <- randomRIO (0, l - 1)
  return $ xs!!i

makeRandomColors :: Display -> Int -> IO [Pixel]
makeRandomColors dpy c = replicateM c $ getRandomColor dpy

getRandomColor :: Display -> IO Pixel
getRandomColor dpy = do
  rs <- replicateM 3 $ randomRIO (0, 255) :: IO [Word8]
  let colorValues = map (padLeft 2 '0')
                    $ map ((flip showHex) "") rs
  let colorString = "rgb:" ++ intercalate "/" colorValues
  initColor dpy colorString

getRandomRect :: [Pixel] -> Dimension -> Dimension -> IO Graphical
getRandomRect colors width height = do
  p@(x, y) <- getRandomCoord width height
  w <- randomRIO (0, width - (fromIntegral x))
  h <- randomRIO (0, height - (fromIntegral y))
  c <- getRandomElem colors
  return $ rectangle (p, w, h, c)

getRandomLine :: [Pixel] -> Dimension -> Dimension -> IO Graphical
getRandomLine colors width height = do
 p1 <- getRandomCoord width height
 p2 <- getRandomCoord width height
 c <- getRandomElem colors
 return $ line (p1, p2, c)

getRandomMultiSegmentLine :: Int -> [Pixel] -> Dimension -> Dimension -> IO [Graphical]
getRandomMultiSegmentLine count colors width height = do
  c <- getRandomElem colors
  coords <- replicateM (count + 1) $ getRandomCoord width height
  return $ map (\(p1, p2) -> line (p1, p2 , c)) $ zip coords $ tail coords

getRandomMultiSegmentRectilinearLine :: Int -> [Pixel] -> Dimension -> Dimension -> IO [Graphical]
getRandomMultiSegmentRectilinearLine count colors width height = do
  c <- getRandomElem colors
  values <- mapM (\x -> randomRIO (0, x)) $ take count $ cycle [fromIntegral width, fromIntegral height]
  let paired = concat $ map (\(a, b) -> [a, b]) $ zip values values
  let points = zip paired $ tail paired
  return $ map (\(p1, p2) -> line (p1, p2, c)) $ zip points $ tail points


getRandomPoint :: [Pixel] -> Dimension -> Dimension -> IO Graphical
getRandomPoint colors width height = do
  p <- getRandomCoord width height
  c <- getRandomElem colors
  return $ point (p, c)


getRandomCoord :: Dimension -> Dimension -> IO Coord
getRandomCoord width height = do
  x <- randomRIO (0, fromIntegral width)
  y <- randomRIO (0, fromIntegral height)
  return (x, y)
