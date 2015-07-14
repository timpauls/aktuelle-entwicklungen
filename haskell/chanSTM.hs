module ChanSTM where
import Control.Concurrent
import Control.Concurrent.STM
import qualified MVar as OwnMVar

type ChanSTM a = OwnMVar.MVar [a]

newChan :: STM (ChanSTM a)
newChan = do
  OwnMVar.newEmptyMVar

writeChan :: ChanSTM a -> a -> STM ()
writeChan ch x = do
  xs <- OwnMVar.tryTakeMVar ch
  case xs of
    (Just v) ->
      OwnMVar.putMVar ch (v ++ [x])
    Nothing ->
      OwnMVar.putMVar ch [x]


readChan :: ChanSTM a -> STM a
readChan ch = do
  xs <- OwnMVar.takeMVar ch
  case xs of
    (y:[])  -> do
      return y
    (y:ys)  -> do
      OwnMVar.putMVar ch ys
      return y


reader :: Show a => ChanSTM a -> IO ()
reader ch = do
  value <- atomically (ChanSTM.readChan ch)
  print value
  reader ch

writer :: ChanSTM a -> a -> IO ()
writer ch value = do
  atomically (ChanSTM.writeChan ch value)
  threadDelay 100000
  writer ch value

main = do
  chan <- atomically ChanSTM.newChan

  forkIO (writer chan 7)
  forkIO (writer chan 13)

  reader chan
