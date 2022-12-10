{-# LANGUAGE TemplateHaskell #-}

module RectangleData (
  root, rects, count,
  nw, ne, se, sw,
  _nw, _ne, _se, _sw,
  n, e, w, s,
  _n, _e, _w, _s,
  newRect,
  Corner,
  NSDir(N, S),
  EWDir(E, W),
  Dir(North, South, East, West),
  Vertex(Vertex),
  Rectangle(Rectangle),
  Rectangulation(Rectangulation),
  Wall
  ) where

import Control.Lens (makeLenses)
import Data.List (intersperse)

data NSDir = N | S
data EWDir = E | W
data Dir = North
           | East
           | South
           | West

newtype Vertex = Vertex Int deriving (Eq, Ord)

instance Show Vertex where
  show (Vertex v) = show v

type Corner = (NSDir, EWDir)
type Wall = [Vertex]

data Rectangle = Rectangle {
  _nw :: Vertex,
  _ne :: Vertex,
  _se :: Vertex,
  _sw :: Vertex,
  _n :: Wall,
  _e :: Wall,
  _w :: Wall,
  _s :: Wall
  }


data Rectangulation = Rectangulation {
  _root :: Rectangle,
  _rects :: [Rectangle],
  _count :: Int
  }

makeLenses ''Rectangulation
makeLenses ''Rectangle

newRect :: Vertex -> Vertex -> Vertex -> Vertex -> Rectangle
newRect v0 v1 v2 v3 = Rectangle v0 v1 v2 v3 [] [] [] []

replace :: Char -> Char -> Char -> Char
replace a b c = if a == c then b else c

pad :: a -> Int -> [a] -> [a]
pad c l cs = replicate (max 0 $ l - length cs) c ++ cs

padr :: a -> Int -> [a] -> [a]
padr c l cs = cs ++ replicate (max 0 $ l - length cs) c

pad_ :: Int -> String -> String
pad_ = pad ' '

padr_ :: Int -> String -> String
padr_ = padr ' '

showHorWall :: Wall -> String
showHorWall = map (replace ' ' '-').unwords.map show

getVertHeight :: Wall -> Int
getVertHeight cs = max (length cs) 1

instance Show Rectangle where
  show (Rectangle _nw _ne _se _sw _n _e _w _s) = do
    let horWidth = maximum.(1:).map (length.showHorWall) $ [_n, _s]
    let vertHeight = maximum.(1:).map getVertHeight $ [_e, _w]
    let wStrs = map show _w
    let eStrs = map show _e
    let pairedVerts = take vertHeight $
          zip (wStrs ++ repeat "|")
          (eStrs ++ repeat "|")

    let [leftWidth, rightWidth] = map (maximum.map (length.show))
          [[_nw, _sw] ++ _w, [_ne, _se] ++ _e]
    let makeRow = makeRow_ leftWidth horWidth rightWidth

    let paddingRow = makeRow ("|", "|")

    let rows = intersperse paddingRow $ map makeRow pairedVerts :: [String]
    let row0 = pad_ leftWidth (show _nw) ++
          "--" ++
          pad '-' horWidth (showHorWall _n) ++
          "--" ++
          padr_ rightWidth (show _ne) :: String
    let rowN = pad_ leftWidth (show _sw) ++
          "--" ++
          pad '-' horWidth (showHorWall _s) ++
          "--" ++
          padr_ rightWidth (show _se) :: String
    unlines $
      [row0, paddingRow] ++
      rows ++
      [paddingRow, rowN]
    where
      makeRow_ :: Int -> Int -> Int -> (String, String) -> String
      makeRow_ leftWidth horWidth rightWidth (l, r) =
        pad_ leftWidth l ++
        pad_ (horWidth + 4) "   " ++
        padr_ rightWidth r



instance Show Rectangulation where
  show (Rectangulation rt rs _) = "\n" ++
    map (replace '|' '*'.replace '-' '*') (show rt) ++ "\n" ++
    unlines (map show rs)
