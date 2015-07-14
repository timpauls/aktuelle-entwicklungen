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

isEmpty :: ChanSTM a -> STM Bool
isEmpty ch = do
  xs <- OwnMVar.tryTakeMVar ch
  case xs of
    (Just v) -> do
      OwnMVar.putMVar ch v
      return False
    Nothing ->
      return True

ungetChan :: ChanSTM a -> a -> STM ()
ungetChan ch x = do
  xs <- OwnMVar.tryTakeMVar ch
  case xs of
    (Just v) ->
      OwnMVar.putMVar ch ([x] ++ v)
    Nothing ->
      OwnMVar.putMVar ch [x]

writeListToChan :: ChanSTM a -> [a] -> STM ()
writeListToChan ch (a:[]) = ChanSTM.writeChan ch a
writeListToChan ch (a:as) = do
  ChanSTM.writeChan ch a
  ChanSTM.writeListToChan ch as



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

  --Test Aufgabe 5a
  --forkIO (writer chan 7)
  --forkIO (writer chan 13)
  --reader chan

  --Test Aufgabe 5b isEmpty
  --atomically (ChanSTM.writeChan chan 5)
  --forkIO (reader chan)
  --threadDelay 500000
  --empty <- (atomically (ChanSTM.isEmpty chan))
  --print empty

  -- Test unget
  --atomically (ChanSTM.writeChan chan 5)
  --forkIO (reader chan)
  --threadDelay 500000
  --atomically (ChanSTM.ungetChan chan 47)

  -- Test writeListToChan
  atomically (ChanSTM.writeListToChan chan [0..20000])
  forkIO (reader chan)
