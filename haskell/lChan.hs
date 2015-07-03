import Control.Concurrent

type LChan a = MVar [a]

newLChan :: IO (LChan)
newLChan = do
  newMVar []

writeLChan :: LChan a -> a -> IO ()
writeLChan ch x = do
  xs <- takeMVar ch
  putMVar ch (xs ++ [x])

readLChan :: LChan a -> IO a
readLChan ch = do
  xs <- takeMVar ch
  case xs of
    (y:ys)  -> do
      writeLChan ys
      return y
    []      -> do
      writeLChan ch xs
      readLChan ch
