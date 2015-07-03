import Control.Concurrent.STM
import Control.Concurrent

type Account = TVar Int

newAccount :: Int -> STM Account
newAccount am = do
  newTVar am

getBalance :: Account -> STM Int
getBalance acc = do
  readTVar acc

deposit :: Account -> Int -> STM ()
deposit acc am = do
  bal <- getBalance acc
  writeTVar acc (bal + am)

transfer :: Account -> Account -> Int -> STM ()
transfer from to am = do
  deposit from (-am)
  deposit to am

main = do
  k1 <- atomically (newAccount 100)
  k2 <- atomically (newAccount 200)
  atomically (transfer k1 k2 50)
  forkIO (do atomically (transfer k2 k1 10))

  getLine
  b1 <- atomically (getBalance k1)
  b2 <- atomically (getBalance k2)

  putStrLn ("K1: " ++ (show b1) ++ "; K2: " ++ (show b2))
