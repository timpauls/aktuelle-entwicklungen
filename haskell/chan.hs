module Main where
import Control.Concurrent

main = startTest 1

startTest :: Int -> IO ()
startTest n = do
	chan <- newChan
	print "Start Thread 1"
	forkIO (reader chan)
	threadDelay 2000000
	print "Start Thread 2"
	forkIO (ungetter chan)
	threadDelay 2000000
	return ()

reader :: Chan Int -> IO ()
reader channel = do
	print "start read"
	result <- readChan channel
	threadDelay 2000000
	print $ "result" ++ show result


ungetter :: Chan Int -> IO ()
ungetter channel = do
	print "start unget"
	unGetChan channel 1337
	threadDelay 2000000
	print "end unget"