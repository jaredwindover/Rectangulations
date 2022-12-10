-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Draw (draw, drawAll, rectangle, line, point, Graphical, Coord) where

import Graphics.X11.Xlib (
  setForeground,
  setLineAttributes,
  fillRectangle,
  drawPoint,
  drawLine,
  lineSolid,
  capButt,
  joinMiter,
  Pixel,
  Position,
  Dimension,
  Display,
  Window,
  GC
  )

-- type Rectangle = (Position, Position, Dimension, Dimension, Pixel)

-- class Graphical a where
--   draw :: Display -> Window -> GC -> a -> IO ()
  -- drawMany :: Display -> Window -> GC -> [a] -> IO ()

type Drawer = Display -> Window -> GC -> IO ()

type Coord = (Position, Position)

type Rectangle = (Coord, Dimension, Dimension, Pixel)
type Line = (Coord, Coord, Pixel)
type Point = (Coord, Pixel)

newtype Graphical = Graphical {
  draw :: Drawer
  }

rectangle :: Rectangle -> Graphical
rectangle r = Graphical {draw = drawRectangle r}

drawRectangle :: Rectangle -> Drawer
drawRectangle ((x, y), w, h, c) dpy win gc = do
  setForeground dpy gc c
  fillRectangle dpy win gc x y w h

line :: Line -> Graphical
line l = Graphical {draw = drawLine_ l}

drawLine_ :: Line -> Drawer
drawLine_ ((x1, y1), (x2, y2), c) dpy win gc = do
  setForeground dpy gc c
  setLineAttributes dpy gc 10 lineSolid capButt joinMiter
  drawLine dpy win gc x1 y1 x2 y2

point :: Point -> Graphical
point p = Graphical {draw = drawPoint_ p}

drawPoint_ :: Point -> Drawer
drawPoint_ ((x, y), c) dpy win gc = do
  setForeground dpy gc c
  drawPoint dpy win gc x y
--
-- instance Graphical Rectangle where
--   draw dpy win gc (x, y, w, h, c) = do
--     setForeground dpy gc c
--     fillRectangle dpy win gc x y w h
--
-- instance Graphical Point where
--   draw dpy win gc (x, y, c) = do
--     setForeground dpy gc c
--     drawPoint dpy win gc x y

drawAll :: Display -> Window -> GC -> [Graphical] -> IO ()
drawAll dpy win gc = mapM_ $ \x -> draw x dpy win gc


-- data Graphic = Rectangle Position Position Dimension Dimension Pixel
--   | Point Position Position Pixel

-- draw :: [Graphic] -> Display -> Window -> GC -> IO ()
-- draw [] _ _ _ = do
--   return ()
--
-- draw ((Rectangle x y w h c):xs) dpy win gc = do
--   setForeground dpy gc c
--   fillRectangle dpy win gc x y w h
--   draw xs dpy win gc
--
-- draw ((Point x y c):xs) dpy win gc = do
--   setForeground dpy gc c
--   drawPoint dpy win gc x y
--   draw xs dpy win gc
