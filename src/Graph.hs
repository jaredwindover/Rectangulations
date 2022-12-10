{-# LANGUAGE TupleSections #-}

module Graph (
  reduce,
  invert,
  getDrawables
  ) where

import Data.List (partition)
import Data.Map (Map, insertWith, fromList, empty, findWithDefault, mapWithKey, (!))
import Data.Set (Set, toList, disjoint, union, unions, singleton)
import qualified Data.Set as S (empty, fromList, map)
import Data.Tuple.Extra (both, second)
import Data.Graph (Graph, graphFromEdges)
import Control.Monad.State.Lazy(StateT, liftIO)
import Control.Lens (use, (^.))

import RectangleData (
  Rectangle(Rectangle), nw, ne, se, sw,
  Vertex,
  Rectangulation, root, rects
  )
import MST (maximalSpanningTree)

type Op = StateT Rectangulation IO

type Coord = (Int, Int)
type DrawableRect = (Coord, Coord, Coord, Coord)

reduce :: Ord a => [Set a] -> [Set a]
reduce [] = []
reduce (x:xs) = do
  let (dj, j) = partition (disjoint x) $ reduce xs
  unions (x:j):dj

invert :: Ord a => [Set a] -> Map a Int
invert vss = fromList
  $ concatMap (toList.withIndex)
  $ zip vss [0..]
  where
    withIndex :: Ord a => Ord b => (Set a, b) -> Set (a, b)
    withIndex (es, i) = S.map (, i) es

getDrawables :: Op ([DrawableRect])
getDrawables = do
  rs <- use rects
  cs <- makeCoordinates
  return $ map (transform cs) rs
  where
    transform :: Map Vertex (Int, Int) -> Rectangle -> DrawableRect
    transform cs r = (
      cs!(r^.nw),
      cs!(r^.ne),
      cs!(r^.se),
      cs!(r^.sw)
      )

makeCoordinates :: Op (Map Vertex (Int, Int))
makeCoordinates = do
  hidxs <- getHorIdxs
  vidxs <- getVertIdxs
  return $ mapWithKey (\ k -> (, vidxs ! k)) hidxs

getHorIdxs :: Op (Map Vertex Int)
getHorIdxs = do
  (g, start, eq) <- getHorGraph
  m <- liftIO $ maximalSpanningTree g start
  liftIO $ print g
  liftIO $ print m
  liftIO $ print eq
  return $ fromList $ concatMap flatten $ zip eq $ map (m!) [0..]

getVertIdxs :: Op (Map Vertex Int)
getVertIdxs = do
  (g, start, eq) <- getVertGraph
  m <- liftIO $ maximalSpanningTree g start
  return $ fromList $ concatMap flatten $ zip eq $ map (m!) [0..]

flatten :: (Set Vertex, Int) -> [(Vertex, Int)]
flatten (vs, i) = toList $ S.map (,i) vs

getHorGraph :: Op (Graph, Int, [Set Vertex])
getHorGraph = do
  r <- use root
  rs <- use rects
  let left = r^.nw
  let constraints = r:rs
  let equivalenceClasses = (reduce.concatMap toEquivalentVert) constraints
  let equivMap = invert equivalenceClasses
  let start = equivMap ! left
  let edgeMap = foldl toMapSet empty $
        map (both (equivMap !)) $
        concatMap getHorEdges rs
  let edges = map (\i -> (i, i, toList $ findWithDefault S.empty i edgeMap)) $ take (length equivalenceClasses) [0..]
  let (g, _, _) = graphFromEdges edges
  return (g, start, equivalenceClasses)

getVertGraph :: Op (Graph, Int, [Set Vertex])
getVertGraph = do
  r <- use root
  rs <- use rects
  let top = r^.nw
  let constraints = r:rs
  let equivalenceClasses = (reduce.concatMap toEquivalentHor) constraints
  let equivMap = invert equivalenceClasses
  let start = equivMap ! top
  let edgeMap = foldl toMapSet empty $
        map (both (equivMap !)) $
        concatMap getVertEdges rs
  let edges = map (\i -> (i, i, toList $ findWithDefault S.empty i edgeMap)) $ take (length equivalenceClasses) [0..]
  let (g, _, _) = graphFromEdges edges
  return (g, start, equivalenceClasses)

toEquivalentHor :: Rectangle -> [Set Vertex]
toEquivalentHor (Rectangle _nw _ne _se _sw _n _e _w _s) =
  map S.fromList[_nw:_ne:_n, _sw:_se:_s]

toEquivalentVert :: Rectangle -> [Set Vertex]
toEquivalentVert (Rectangle _nw _ne _se _sw _n _e _w _s) =
  map S.fromList[_nw:_sw:_w, _ne:_se:_e]

getHorEdges :: Rectangle -> [(Vertex, Vertex)]
getHorEdges (Rectangle _nw _ne _se _sw _n _e _w _s) =
  concatMap toEdges[_nw: _n ++ [_ne], _sw: _s ++ [_se]]

getVertEdges :: Rectangle -> [(Vertex, Vertex)]
getVertEdges (Rectangle _nw _ne _se _sw _n _e _w _s) =
  concatMap toEdges [_nw: _w ++ [_sw], _ne: _e ++ [_se]]

toEdges :: [a] -> [(a, a)]
toEdges xs = zip xs $ drop 1 xs

toMapSet :: Ord a => Ord b => Map a (Set b) -> (a, b) -> Map a (Set b)
toMapSet m (a, b) = insertWith union a (singleton b) m
