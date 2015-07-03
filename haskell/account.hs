import Control.Concurrent

type Account = MVar Int

getBalance :: Account -> IO Int
getBalance acc = do
  readMVar acc

deposit :: Account -> Int -> IO ()
deposit acc am = do
  bal <- takeMVar acc
  putMVar acc (bal + am)

transfer :: Account -> Account -> Int -> IO ()
transfer from to am = do
  fromBal <- takeMvar from
  maybeToBal <- tryTakeMVar to
  case maybeToBal of
    Nothing -> do
      putMVar from fromBal
      transfer from to am
    Just toBal -> do
      putMVar from  (fromBal - am)
      putMVar to    (toBal + am)

transfer :: Account -> Accoutn -> Int -> IO ()
transfer from to am =
  if from < to then do
    fromBal <- takeMvar from
    toBal   <- takeMvar to
    putMVar from  (fromBal - am)
    putMVar to    (toBal + am)
  else do
    toBal   <- takeMvar to
    fromBal <- takeMvar from
    putMVar from  (fromBal - am)
    putMVar to    (toBal + am)

limitedTransfer :: Account -> Account -> Int -> IO ()
limitedTransfer from to am = do
  fromBal <- takeMvar from
  if fromBal > am then do
    putMVar from  (fromBal - am)
    toBal   <- takeMvar to
    putMVar to    (toBal + am)
    return True
  else do
    putMVar from fromBal
    return False

limitedCollectedTransfer :: [Account] -> Account -> Int -> IO Bool
limitedCollectedTransfer
