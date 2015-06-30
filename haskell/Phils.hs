module Main where

import Control.Concurrent

main = startPhil 5

startPhil :: Int -> IO ()
startPhil n = do
  sticks <- mapM (const (newMVar ())) [1..n]
  mapM_ (\ (n,(l,r)) -> forkIO (phil n l r))
        (zip [1..] (zip sticks (tail sticks)))
  phil 0 (last sticks) (head sticks)

phil :: Int -> MVar () -> MVar () -> IO ()
phil n l r = do
  takeMVar l

  out <- tryTakeMVar r
  case out of
    Just () -> do
      print n
      putMVar l ()
      putMVar r ()
      phil n l r
    Nothing -> do
      putMVar l ()
      phil n l r