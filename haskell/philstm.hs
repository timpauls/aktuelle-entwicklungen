module Main where
import Control.Concurrent
import Control.Concurrent.STM

main = do
	s1 <- atomically newStick
	s2 <- atomically newStick
	s3 <- atomically newStick
	s4 <- atomically newStick
	s5 <- atomically newStick

	forkIO $ phil 1 s1 s2
	forkIO $ phil 2 s2 s3
	forkIO $ phil 3 s3 s4
	forkIO $ phil 4 s4 s5

	phil 5 s5 s1

type Stick = TVar Bool

newStick :: STM Stick
newStick = do
	newTVar True

takeStick :: Stick -> STM ()
takeStick stick = do
	b <- readTVar stick
	if b then
		writeTVar stick False
	else
		retry

putStick :: Stick -> STM ()
putStick stick = do
	writeTVar stick True

phil :: Int -> Stick -> Stick -> IO ()
phil n l r = do
	atomically $ do
		takeStick l
		takeStick r
		
	print n
	atomically $ do 
		putStick l
		putStick r

	phil n l r
