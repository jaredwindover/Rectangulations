{-# LANGUAGE TemplateHaskell #-}

module MST (maximalSpanningTree) where

import Prelude hiding (lookup)
import Data.Graph (Graph, edges)
import Data.Map (Map, empty, insert, lookup, (!))
import Data.Maybe (isJust, fromJust)
import Data.Tuple.Extra (second)
import Control.Monad.State.Lazy(MonadIO, liftIO, gets, get)
import Control.Monad.Trans.State.Lazy(StateT, evalStateT)
import Control.Lens (
  (^.), (%=), (.=),
  use,
  makeLenses
  )

import RectangleData (
  Rectangle(Rectangle), nw,
  Vertex,
  Rectangulation, root, rects
  )

data MSTState = MSTState {_g:: Graph, _m:: Map Int Int, _n:: [Int]} deriving (Show)

type Op = StateT MSTState IO

makeLenses ''MSTState

mprint :: MonadIO m => Show a => a -> m ()
mprint = liftIO.print

printState :: Op ()
printState = do
  s <- get
  mprint $ take 10 $ repeat '-'
  mprint s
  mprint $ take 10 $ repeat '-'
  return ()

initialMSTState :: Graph -> MSTState
initialMSTState g = MSTState g empty []

maximalSpanningTree :: Graph -> Int -> IO (Map Int Int)
maximalSpanningTree graph s = evalStateT (
  do
    m %= insert s 0
    n .= [s]
    rstep
  ) $ initialMSTState graph

rstep :: Op (Map Int Int)
rstep = do
  isEmpty <- gets $ null.(^.n)
  if isEmpty then use m else step >> rstep

step :: Op ()
step = do
  cg <- use g
  cm <- use m
  s <- gets $ head.(^.n)
  n %= tail

  let vs = outNodes s cg
  let inns = map (`inNodes` cg) vs :: [[Int]]
  let scores = map (map $ flip lookup cm) inns :: [[Maybe Int]]
  let mscores = map sequence scores :: [Maybe [Int]]
  let maxes = map (fmap maximum) mscores :: [Maybe Int]
  let maxes' = zip vs maxes
  let nvs = map (second fromJust) $ filter (isJust . snd) maxes'
  m %= (flip $ foldl (\m' (a, score) -> insert a (score + 1) m')) nvs
  n %= (map fst nvs ++)
  return ()

outEdges :: Int -> Graph -> [(Int, Int)]
outEdges i = filter ((==i).fst).edges

inEdges :: Int -> Graph -> [(Int, Int)]
inEdges i = filter ((==i).snd).edges

outNodes :: Int -> Graph -> [Int]
outNodes i = map snd.outEdges i

inNodes :: Int -> Graph -> [Int]
inNodes i = map fst.inEdges i
