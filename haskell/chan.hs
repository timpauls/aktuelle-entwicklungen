module Main where
import Control.Concurrent

main = do
	startTestUnget 1
	threadDelay 2000000
	startTestEmpty 1

startTestEmpty :: Int -> IO ()
startTestEmpty n = do
	chan <- newChan
	print "Start Thread Reader"
	forkIO $ reader chan
	threadDelay 2000000
	print "Start Thread Empty"
	forkIO $ empty chan
	threadDelay 2000000
	return ()

startTestUnget :: Int -> IO ()
startTestUnget n = do
	chan <- newChan
	print "Start Thread Reader"
	forkIO $ reader chan
	threadDelay 2000000
	print "Start Thread Unget"
	forkIO $ ungetter chan
	threadDelay 2000000
	return ()

reader :: Chan Int -> IO ()
reader channel = do
	print "start read"
	result <- readChan channel
	threadDelay 2000000
	print $ "result " ++ show result


ungetter :: Chan Int -> IO ()
ungetter channel = do
	print "start unget"
	unGetChan channel 1337
	threadDelay 2000000
	print "end unget"

empty :: Chan Int -> IO ()
empty channel = do
	print "start isEmpty"
	result <- isEmptyChan channel
	threadDelay 2000000
	print $ "result " ++ show result