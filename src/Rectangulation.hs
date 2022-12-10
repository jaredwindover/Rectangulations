{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Rectangulation (
  initialRectangulation,
  vertex,
  withEdge,
  withCorner,
  withCornerOrEdge,
  hasCorner,
  hasEdge,
  newVertex,
  newRect,
  addRectangle,
  getRectangleFromCorner,
  getCornerVertices,
  getVertices,
  getCandidateInsertions,
  insertRectangle,
  Side (Right, Down)
  ) where

-- import Random (getRandomElem)
import Prelude hiding(Right)
import Control.Exception (assert)
import Control.Monad (join)
import Control.Monad.Trans(MonadIO)
import Control.Monad.State.Lazy (MonadState, put, get, gets, liftIO)

import Control.Lens (
  Lens',
  Fold,
  Traversal',
  use, view,
  traverse, filtered, folded, each,
  dropping, droppingWhile,
  toListOf,
  preuse,
  (??), (.=), (^.), (|>), (%=), (.~), (%~), (<|), (|>),
  (^..)
  )
import Data.Function ((&))
import Data.List ((\\))
import Data.Map (Map, insert, empty)

import RectangleData
data Side = Right | Down deriving (Eq, Ord, Show)

type RM m = MonadState Rectangulation m


initialRectangulation :: Rectangulation
initialRectangulation = do
  let v0 = Vertex 0
  let v1 = Vertex 1
  let v2 = Vertex 2
  let v3 = Vertex 3
  let _root = newRect v0 v1 v2 v3
  Rectangulation _root [_root] 4

vertex :: Corner -> Lens' Rectangle Vertex
vertex (N, E) = ne
vertex (N, W) = nw
vertex (S, E) = se
vertex (S, W) = sw

forVertex :: Applicative f => Vertex -> (Rectangle -> f Rectangle) -> [Rectangle] -> f [Rectangle]
forVertex v = traverse.filtered containsV where
  containsV = flip rectangleContainsVertex v

withCornerOrEdge :: Applicative f => Corner -> Dir -> Vertex -> (Rectangle -> f Rectangle) -> Rectangle -> f Rectangle
withCornerOrEdge c d v = filtered $ hasCornerOrEdge c d v

withCorner :: Applicative f => Corner -> Vertex -> (Rectangle -> f Rectangle) -> Rectangle -> f Rectangle
withCorner c v = filtered $ hasCorner c v

withEdge ::Applicative f => Dir -> Vertex -> (Rectangle -> f Rectangle) -> Rectangle -> f Rectangle
withEdge d v = filtered $ hasEdge d v

hasCornerOrEdge :: Corner -> Dir -> Vertex -> Rectangle -> Bool
hasCornerOrEdge c d v = flip any [hasCorner c v, hasEdge d v].(&)

hasCorner :: Corner -> Vertex -> Rectangle -> Bool
hasCorner c v = (==v).flip _getVertex c

hasEdge :: Dir -> Vertex -> Rectangle -> Bool
hasEdge d v = (v `elem`).case d of
     North -> _n
     East -> _e
     South -> _s
     West -> _w

_getVertex :: Rectangle -> Corner -> Vertex
_getVertex r c = case c of (N, W) -> _nw r
                           (N, E) -> _ne r
                           (S, E) -> _se r
                           (S, W) -> _sw r

getRectangleFromCorner :: RM m => Corner -> m (Maybe Rectangle)
getRectangleFromCorner corner = do
  v <- use $ root.vertex corner
  preuse $ rects.forVertex v.withCorner corner v


newVertex :: RM m => m Vertex
newVertex = do
  (Rectangulation rt rs c) <- get
  put $ Rectangulation rt rs (c + 1)
  return $ Vertex c

addRectangle :: RM m => Rectangle -> m ()
addRectangle r = do
  (Rectangulation rt rs c) <- get
  put $ Rectangulation rt (r:rs) c
  return ()

getCornerVertices :: Rectangle -> [Vertex]
getCornerVertices = ([_ne, _nw, _se, _sw] ??)

getWallVertices :: Rectangle -> [Vertex]
getWallVertices = concat.([_n, _e, _w, _s] ??)

getVertices :: Rectangle -> [Vertex]
getVertices = concat.([getCornerVertices, getWallVertices] ??)

rectangleContainsVertex :: Rectangle -> Vertex -> Bool
rectangleContainsVertex r v = elem v $ getVertices r

getCandidateInsertions :: RM m => m (Map Side [Vertex])
getCandidateInsertions = do
  rs <- getRightCandidateInsertions
  ds <- getDownCandidateInsertions
  return $ insert Right rs $ insert Down ds empty

getRightCandidateInsertions :: RM m => m [Vertex]
getRightCandidateInsertions =  do
  r <- use root
  rightRects <- fmap concat $ mapM (\v -> gets (^.. rects.traverse.withCorner (N, E) v)) $ r^.e
  let nes = (r:rightRects)^..traverse.ne
  let ns = join $ rightRects^..traverse.n
  return $ nes ++ ns

getDownCandidateInsertions :: RM m => m [Vertex]
getDownCandidateInsertions = do
  r <- use root
  downRects <- fmap concat $ mapM (\v -> gets (^.. rects.traverse.withCorner (S, W) v)) $ r^.s
  let sws = (r:downRects)^..traverse.sw
  let ws = join $ downRects^..traverse.w
  return $ sws ++ ws

bsnoc :: a -> [a] -> [a]
bsnoc x xs = xs ++ [x]

insertRectangle :: RM m => MonadIO m => Side -> Vertex -> m ()
insertRectangle Right v = do
  rt <- use root
  let bottomLeft = rt^.vertex (S, E)
  r <- gets (^.. rects.traverse.withCornerOrEdge (N, E) North v)
  let topRight = assert (length r == 1) (head r)^.vertex (N, E)
  let newN = r^..traverse.n.dropping 1 (droppingWhile (/=v) folded)
  let dropEast = if hasCorner (N, E) topRight rt
        then rt^..e.folded
        else rt^..e.dropping 1 (droppingWhile (/=topRight) folded)
  topLeft <- newVertex
  bottomRight <- newVertex
  rects.traverse.withCorner (S, E) v.s.= [topLeft]
  rects.traverse.withCorner (N, E) topRight.ne .= topLeft
  root.withCorner (N, E) topRight.n %= bsnoc topLeft
  addRectangle $ newRect topLeft topRight bottomRight bottomLeft
    & n.~newN
    & w.~dropEast
  rects.traverse.withCornerOrEdge (N, E) North v.n %= (\\) newN
  root.e %= (\\) dropEast
  rt <- use root
  let updateRootE = if hasCorner (N, E) topRight rt
        then id
        else bsnoc topRight
  root.e %= updateRootE
  root.se.= bottomRight
  root.s %= bsnoc bottomLeft

insertRectangle Down v = do
  rt <- use root
  let topRight = rt^.vertex (S, E)
  r <- gets (^.. rects.traverse.withCornerOrEdge (S, W) West v)
  let bottomLeft = assert (length r == 1) (head r)^.vertex (S, W)
  let newW = r^..traverse.w.dropping 1 (droppingWhile (/=v) folded)
  let dropSouth = if hasCorner (S, W) bottomLeft rt
        then rt^..s.folded
        else rt^..s.dropping 1 (droppingWhile (/=bottomLeft) folded)
  bottomRight <- newVertex
  topLeft <- newVertex
  rects.traverse.withCorner (S, E) v.e.= [topLeft]
  rects.traverse.withCorner (S, W) bottomLeft.sw .= topLeft
  root.withCorner (S, W) bottomLeft.w %= bsnoc topLeft
  addRectangle $ newRect topLeft topRight bottomRight bottomLeft
    & w.~newW
    & n.~dropSouth
  rects.traverse.withCornerOrEdge (S, W) West v.w %= (\\) newW
  root.s %= (\\) dropSouth
  rt <- use root
  let updateRootS = if hasCorner (S, W) bottomLeft rt then id else bsnoc bottomLeft
  root.s %= updateRootS
  root.se.= bottomRight
  root.e %= bsnoc topRight
