module Main where
import Control.Concurrent

main = do
	s1 <- newStick
	s2 <- newStick
	s3 <- newStick
	s4 <- newStick
	s5 <- newStick

	forkIO $ phil 1 s1 s2
	forkIO $ phil 2 s2 s3
	forkIO $ phil 3 s3 s4
	forkIO $ phil 4 s4 s5

	phil 5 s5 s1

type Stick = MVar ()

newStick :: IO Stick
newStick = do
	newMVar ()

takeStick :: Stick -> IO ()
takeStick stick = do
	takeMVar stick

putStick :: Stick -> IO ()
putStick stick = do
	putMVar stick ()

phil :: Int -> Stick -> Stick -> IO ()
phil n l r = do
	takeStick l
	takeStick r
	print n
	putStick l
	putStick r
	phil n l r
