{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Thing (
  In (In), _a, _b, a, b, Tng(Tng), _xs, xs,
  testTng
  ) where

import Control.Lens
import Control.Monad.State.Lazy (liftIO, gets)
import Control.Monad.Trans.State.Lazy(StateT, evalStateT)

data In = In {_a:: Int, _b:: Int} deriving (Eq, Ord, Show)

data Tng = Tng {_xs:: [In]} deriving (Eq, Ord, Show)

makeLenses ''Tng
makeLenses ''In

testTng :: Tng
testTng = Tng{_xs = [In {_a = 0, _b = 2}, In {_a = 1, _b = 3}]}

a0Ins :: Applicative f => (Int -> f Int) -> Tng -> f Tng
a0Ins = xs.y.b

y :: (Traversable t, Applicative f) => (In -> f In) -> t In -> f (t In)
y = traverse.filtered ((==0)._a)

f :: StateT Tng IO ()
f = do
  got <- gets $ (^.. a0Ins)
  liftIO $ putStrLn $ show got
  return ()

g :: IO ()
g = do
  evalStateT f testTng

--evenAs :: Lens' Tng In
--evenAs = xs.traverse.filtered (even._a)
