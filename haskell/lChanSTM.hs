import Control.Concurrent.STM
import Control.Concurrent

type LChan a = TVar [a]

newLChan :: STM (LChan a)
newLChan = do
  newTVar []

writeLChan :: LChan a -> a -> STM ()
writeLChan ch x = do
  xs <- readTVar ch
  writeTVar ch (xs ++ [x])

readLChan :: LChan a -> STM a
readLChan ch = do
  xs <- readTVar ch
  case xs of
    (y:ys)  -> do
      writeTVar ch ys
      return y
    []      -> do
      retry

reader :: Show a => LChan a -> IO ()
reader ch = do
  value <- atomically (readLChan ch)
  print value
  reader ch

writer :: LChan a -> a -> IO ()
writer ch value = do
  atomically (writeLChan ch value)
  threadDelay 100000
  writer ch value

main = do
  lchan <- atomically newLChan

  forkIO (writer lchan 7)
  forkIO (writer lchan 13)

  reader lchan
