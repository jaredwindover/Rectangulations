module Main (main) where
import Prelude hiding(repeat, lines)

import Graphics.X11.Xlib (
  rootWindow,
  mapWindow,
  openDisplay,
  defaultScreen,
  defaultScreenOfDisplay,
  sync,
  createGC,
  freeGC,

  Pixel,
  Dimension,
  Display,
  Window
  )
import Control.Monad (replicateM)
import Control.Monad.HT (repeat)
import Random (
  makeRandomColors,
  getRandomRect,
  getRandomLine,
  getRandomElem,
  getRandomMultiSegmentLine,
  getRandomMultiSegmentRectilinearLine,
  getRandomPoint,
  getRandomRectangulation,
  )
import RectangleData (Rectangulation, rects)

import Window (mkUnmanagedWindow)
import Draw (Rectangle, Graphical, drawAll, rectangle)
import Graph (DrawableRect, getDrawables)

num_colors :: Int
num_colors = 5

dimensions :: Dimension
dimensions = 1000

num_rects :: Int
num_rects = 75

num_lines :: Int
num_lines = 20

num_points :: Int
num_points = 100000

main :: IO ()
main =
    do dpy <- openDisplay ""
       let dflt = defaultScreen dpy
           scr = defaultScreenOfDisplay dpy
       rootw  <- rootWindow dpy dflt
       win <- mkUnmanagedWindow dpy scr rootw 0 0 dimensions dimensions
       mapWindow dpy win
       sync dpy False
       _ <- repeat $ loop dpy win
       return ()

loop :: Display -> Window -> IO ()
loop dpy win = do
  colors <- makeRandomColors dpy num_colors
  drawInWin dpy win colors
  sync dpy False
  _ <- getLine
  return ()

scale :: Int -> DrawableRect -> DrawableRect
scale c ((x0, y0), (x1, y1), (x2, y2), (x3, y3)) = (
  (c*x0, c*y0),
  (c*x1, c*y1),
  (c*x2, c*y2),
  (c*x3, c*y3))

drawInWin :: Display -> Window -> [Pixel] -> IO ()
drawInWin dpy win colors = do
  gc <- createGC dpy win
  drects <- getRandomRectangulation num_rects
  let scaled_rects = map (scale 30) drects
  rects <- mapM (`makeRect` colors) scaled_rects
  --rects <- replicateM num_rects $ getRandomRect (tail colors) dimensions dimensions
  --lines <- replicateM num_lines $ getRandomLine (tail colors) dimensions dimensions
  -- multiLine <- getRandomMultiSegmentRectilinearLine num_lines [colors!!0] dimensions dimensions
  -- points <- replicateM num_points $ getRandomPoint (tail colors) dimensions dimensions
  drawAll dpy win gc (rectangle ((0, 0), dimensions, dimensions, colors!!0)
                      :(rects))-- ++ multiLine))
  freeGC dpy gc

makeRect :: DrawableRect -> [Pixel] -> IO Graphical
makeRect ((x, y), _, (x', y'), _) colors = do
  c <- getRandomElem colors
  return $ rectangle (
    (fromIntegral x, fromIntegral y),
    fromIntegral $ x' - x,
    fromIntegral $ y' - y,
    c)
