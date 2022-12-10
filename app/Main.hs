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
  getRandomMultiSegmentLine,
  getRandomMultiSegmentRectilinearLine,
  getRandomPoint,
  )

import Window (mkUnmanagedWindow)
import Draw (drawAll, rectangle)

num_colors :: Int
num_colors = 5

dimensions :: Dimension
dimensions = 1000

num_rects :: Int
num_rects = 10

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

drawInWin :: Display -> Window -> [Pixel] -> IO ()
drawInWin dpy win colors = do
  gc <- createGC dpy win
  rects <- replicateM num_rects $ getRandomRect (tail colors) dimensions dimensions
  --lines <- replicateM num_lines $ getRandomLine (tail colors) dimensions dimensions
  multiLine <- getRandomMultiSegmentRectilinearLine num_lines [colors!!0] dimensions dimensions
  -- points <- replicateM num_points $ getRandomPoint (tail colors) dimensions dimensions
  drawAll dpy win gc (rectangle ((0, 0), dimensions, dimensions, colors!!0)
                      :(rects ++ multiLine))
  freeGC dpy gc
