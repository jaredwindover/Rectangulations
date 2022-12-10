module Color (initColor) where

import Graphics.X11.Xlib hiding (Rectangle)

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let dflt = defaultScreen dpy
      colorMap = defaultColormap dpy dflt
  (approx, _) <- allocNamedColor dpy colorMap color
  return $ color_pixel approx
