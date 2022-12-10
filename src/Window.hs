module Window (mkManagedWindow, mkUnmanagedWindow) where

import Data.Bits ((.|.))
import Graphics.X11.Xlib hiding (Rectangle)
import Color (initColor)

mkManagedWindow :: Display
                  -> Screen
                  -> Window
                  -> Position
                  -> Position
                  -> Dimension
                  -> Dimension
                  -> IO Window
mkManagedWindow  dpy scr rw x y w h = do
  let visual = defaultVisualOfScreen scr
      depth = defaultDepthOfScreen scr
      attrMask = cWBackPixel .|. cWBorderPixel
  backgroundColor <- initColor dpy "white"
  borderColor <- initColor dpy "black"
  win <- allocaSetWindowAttributes $ \attributes -> do
    set_background_pixel attributes backgroundColor
    set_border_pixel attributes borderColor
    createWindow dpy rw x y w h 1 depth inputOutput visual attrMask attributes
  return win

mkUnmanagedWindow :: Display
                  -> Screen
                  -> Window
                  -> Position
                  -> Position
                  -> Dimension
                  -> Dimension
                  -> IO Window
mkUnmanagedWindow  dpy scr rw x y w h = do
  let visual = defaultVisualOfScreen scr
      depth = defaultDepthOfScreen scr
      attrMask = cWOverrideRedirect .|. cWBackPixel .|. cWBorderPixel
  backgroundColor <- initColor dpy "white"
  borderColor <- initColor dpy "black"
  win <- allocaSetWindowAttributes $ \attributes -> do
    set_override_redirect attributes True
    set_background_pixel attributes backgroundColor
    set_border_pixel attributes borderColor
    createWindow dpy rw x y w h 1 depth inputOutput visual attrMask attributes
  return win
