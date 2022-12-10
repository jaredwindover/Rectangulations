{-# LANGUAGE TupleSections #-}

module Main(main) where
import Prelude hiding(Right)
import Test.HUnit
import Data.List(sortBy)
import Data.List.Extra (notNull)
import Data.Set (fromList)
import Data.Map (elems, assocs, mapWithKey, findWithDefault, (!))
import Data.Tuple.Extra(both)
import qualified Data.Map as M(fromList)
import Data.Maybe (fromJust)
import Control.Monad(replicateM_)
import Control.Monad.State.Lazy (liftIO, get, gets)
import Control.Monad.Trans.State.Lazy(StateT, evalStateT)
import Control.Lens (set, view, use, filtered, (&), (%=), (.=), (^.), (^..))

import Rectangulation
import RectangleData
import Graph
import Random

type Op = StateT Rectangulation IO

opAssertBool :: String -> Bool -> Op ()
opAssertBool str = liftIO . assertBool str

opAssertEqual :: (Eq a, Show a) => String -> a -> a -> Op ()
opAssertEqual str a1 = liftIO . assertEqual str a1

printState :: Op()
printState = (liftIO.print) =<< get

allFour :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
allFour f (a0, a1, a2, a3) = (f a0, f a1, f a2, f a3)

printNiceState :: Int -> Op()
printNiceState scale = do
  ds <- getDrawables
  let ds' = map (allFour (both (*scale))) ds
  let (maxy, maxx) = both (*scale) $
        head $ sortBy (\(a0, a1) (b0, b1) -> if ((compare b0 a0) == EQ) then compare b1 a1 else compare b0 a0) $
        map (\(_,_,a,_) -> a) ds
  let m = M.fromList $ concatMap (\(nw, ne, se, sw) ->
        (map ((,'-').(,snd nw)) [(fst nw) + 1..(fst ne) - 1]) ++
        (map ((,'-').(,snd sw)) [(fst sw) + 1..(fst se) - 1]) ++
        (map ((,'|').(fst nw,)) [(snd nw) + 1..(snd sw) - 1]) ++
        (map ((,'|').(fst ne,)) [(snd ne) + 1..(snd se) - 1])) ds' ++
        concatMap (\(nw, ne, se, sw) -> (map (, '+') [nw, ne, se, sw])) ds'
  liftIO $ putStrLn $ unlines $ "": map (map (\c -> findWithDefault ' ' c m)) [[(y, x) | y <- [0..maxy]] |  x <- [0..maxx]]

rectsWithEdge :: Vertex -> Dir -> Op [Rectangle]
rectsWithEdge v d = gets (^.. (rects.traverse.filtered (hasInEdge v d)))

hasInEdge :: Vertex -> Dir -> Rectangle -> Bool
hasInEdge v North = flip any [hasCorner (N, W) v,
                              hasCorner (N, E) v,
                              hasEdge North v].(&)
hasInEdge v South = flip any [hasCorner (S, W) v,
                              hasCorner (S, E) v,
                              hasEdge South v].(&)
hasInEdge v East = flip any [hasCorner (N, E) v,
                              hasCorner (S, E) v,
                              hasEdge East v].(&)
hasInEdge v West = flip any [hasCorner (N, W) v,
                              hasCorner (S, W) v,
                              hasEdge West v].(&)

test1 :: Op ()
test1 = do
    _nw <- use $ root.vertex (N, W)
    _ne <- use $ root.vertex (N, E)
    _sw <- use $ root.vertex (S, W)
    _se <- use $ root.vertex (S, E)
    opAssertBool "NW == NE" (_nw /= _ne)
    opAssertBool "Finds Rectangle NE North" . notNull =<< rectsWithEdge _ne North
    opAssertBool "Finds Rectangle NE East" . notNull =<< rectsWithEdge _ne East
    opAssertBool "Finds Rectangle SE South" . notNull =<< rectsWithEdge _se South
    opAssertBool "Finds Rectangle SE East" . notNull =<< rectsWithEdge _se East
    opAssertBool "Finds Rectangle NW North" . notNull =<< rectsWithEdge _nw North
    opAssertBool "Finds Rectangle NW West" . notNull =<< rectsWithEdge _nw West
    opAssertBool "Finds Rectangle SW South" . notNull =<< rectsWithEdge _sw South
    opAssertBool "Finds Rectangle SW West" . notNull =<< rectsWithEdge _sw West
    return ()


--- v0-v1-v4
--- |  |  |
--- v3-v2-v5
test2 :: Op ()
test2 = do
  v0 <- newVertex
  v1 <- newVertex
  v2 <- newVertex
  v3 <- newVertex
  addRectangle $ newRect v0 v1 v2 v3
  opAssertBool "v0 is not South for anything" . null =<< rectsWithEdge v0 South
  opAssertBool "v0 is not East for anything" . null =<< rectsWithEdge v0 East
  opAssertBool "v1 is not West for anything" . null =<< rectsWithEdge v1 West
  opAssertBool "v2 is not West for anything" . null =<< rectsWithEdge v2 West
  v4 <- newVertex
  v5 <- newVertex
  addRectangle $ newRect v1 v4 v5 v2
  opAssertBool "v1 is West for 1 rectangle" . notNull =<< rectsWithEdge v1 West
  opAssertBool "v2 is West for 1 rectangle" . notNull =<< rectsWithEdge v2 West
  opAssertBool "v1 is North for 2 rectangles" . (==2).length =<< rectsWithEdge v1 North
  opAssertBool "v2 is South for 2 rectangles" . (==2).length =<< rectsWithEdge v2 South
  return ()


--- v0-v1-v4
--- |  |  |
--- v3-v2-v5
--- |     |
--- v6----v7
test3 :: Op ()
test3 = do
  v0 <- newVertex
  v1 <- newVertex
  v2 <- newVertex
  v3 <- newVertex
  (.=) rects []
  addRectangle $ newRect v0 v1 v2 v3
  v4 <- newVertex
  v5 <- newVertex
  addRectangle $ newRect v1 v4 v5 v2
  v6 <- newVertex
  v7 <- newVertex
  addRectangle $ newRect v3 v5 v7 v6
  (%=) rects (map (\r -> if view sw r == v6 then set n [v2] r else r))
  opAssertBool "v2 should be part of 3 rectangles" .
    (==3).length.filter ((v2 `elem`).getVertices)
    =<< use rects

  return ()

test4 :: Op()
test4 = do
  _ne <- use $ root.vertex (N, E)
  _sw <- use $ root.vertex (S, W)
  vs <- getCandidateInsertions
  opAssertEqual "initialCandidates are correct" (fromList.concat.elems $ vs) (fromList [_ne, _sw])
  insertRectangle Right _ne
  rnw <- fromJust <$> getRectangleFromCorner (N, W)
  rne <- fromJust <$> getRectangleFromCorner (N, E)
  let n1 = rne^.vertex (N, W)
  let n2 = rnw^.vertex (N, E)
  let s1 = rne^.vertex (S, W)
  let s2 = rnw^.vertex (S, E)
  opAssertEqual "shared N" n1 n2
  opAssertEqual "shared S" s1 s2

  return ()

test5 :: Op()
test5 = do
  _root <- use $ root
  let _sw = _root^.vertex (S, W)
  insertRectangle Down _sw
  rnw <- fromJust <$> getRectangleFromCorner (N, W)
  rsw <- fromJust <$> getRectangleFromCorner (S, W)
  let e1 = rnw^.vertex (S, E)
  let e2 = rsw^.vertex (N, E)
  let w1 = rnw^.vertex (S, W)
  let w2 = rsw^.vertex (N, W)
  opAssertEqual "shared E" e1 e2
  opAssertEqual "shared W" w1 w2
  vs <- getCandidateInsertions
  _root <- use $ root
  let _ne = _root^.vertex (N, E)
  let _sw = _root^.vertex (S, W)
  liftIO $ (fromList.concat.elems $ vs) @?= fromList [_ne, _sw, e1]
  return ()

test6 :: Op ()
test6 = do
  _root <- use $ root
  let rootNE = _root^.vertex (N, E)
  let rootSE = _root^.vertex (S, E)
  let rootSW = _root^.vertex (S, W)
  insertRectangle Right rootNE
  vs <- getCandidateInsertions
  _root <- use $ root
  let newRootNE = _root^.vertex (N, E)
  liftIO $ (fromList.concat.elems $ vs) @?= fromList [newRootNE, rootSE, rootSW]
  return ()

test7 :: Op ()
test7 = do
  _root <- use $ root
  let rootNE = _root^.vertex (N, E)
  let rootSW = _root^.vertex (S, W)
  insertRectangle Right rootNE
  _root <- use $ root
  let newRootNE = _root^.vertex (N, E)
  let newRootE = _root^.vertex (S, E)
  let newRootS = _root^.s
  insertRectangle Down rootSW
  newRootSE <- use $ root.vertex (S, E)
  newRootSW <- use $ root.vertex (S, W)
  seRects <- gets (^..rects.traverse.withCorner (S, E)  newRootSE)
  liftIO $ (head seRects^.n) @?= newRootS
  vs <- getCandidateInsertions
  liftIO $ (fromList.concat.elems $ vs) @?= fromList [newRootNE, newRootE, newRootSW, head newRootS]
  return ()

{--
0--4--1
|  |  |
3--2--5

0--4--1
|  |  |
7--2--5
|     |
3-----6

0--4------1
|  |      |
7--2--8---5
|     |   |
3-----6---9

 0--4-----1
 |  |     |
 7--2--8--5
 |     |  |
11-----6--9
 |        |
 3-------10
--}
test8 :: Op ()
test8 = do
  -- printNiceState 3
  rootNE <- gets (^.root.vertex (N, E))
  insertRectangle Right rootNE
  -- printNiceState 3
  rootSW <- gets (^.root.vertex (S, W))
  insertRectangle Down rootSW
  -- printNiceState 3
  rootE <- head <$> gets (^.root.e) :: Op Vertex
  insertRectangle Right rootE
  -- printNiceState 3
  rootSW <- gets (^.root.vertex (S, W))
  insertRectangle Down rootSW
  -- printNiceState 3
  return ()

test9 :: Op ()
test9 = do
  insertRectangle Right $ Vertex 1
  insertRectangle Down $ Vertex 2
  printNiceState 3

test10 :: Op ()
test10 = do
  replicateM_ 100 step
  printNiceState 3

step :: Op()
step = do
  vs <- getCandidateInsertions
  let vs' = concatMap (\(d, vs) -> map (d,) vs) $ assocs vs
  (d, v) <- getRandomElem vs'
  -- liftIO $ print (d, v)
  insertRectangle d v
  -- printNiceState 3


testPrint :: Op ()
testPrint = do
  let rt = Rectangle (Vertex 0) (Vertex 123) (Vertex 2) (Vertex 345) [Vertex 10, Vertex 11] [Vertex 23] [Vertex 3456, Vertex 27] [Vertex 234, Vertex 45]
  liftIO.print $ Rectangulation rt [rt] 0

main :: IO Counts
main = do
  runTestTT $
    TestList $
      map
        (TestCase . (`evalStateT` initialRectangulation))
        [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10
         -- ,testPrint
        ]
