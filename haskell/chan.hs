module Main where
import Control.Concurrent

main = do
	startTestUnget
	threadDelay 2000000
	startTestEmpty

startTestEmpty :: IO ()
startTestEmpty = do
	chan <- newChan
	putStrLn "Start Thread Reader"
	forkIO $ reader chan
	threadDelay 2000000
	putStrLn "Start Thread Empty"
	forkIO $ empty chan
	threadDelay 2000000
	return ()

startTestUnget :: IO ()
startTestUnget = do
	chan <- newChan
	putStrLn "Start Thread Reader"
	forkIO $ reader chan
	threadDelay 2000000
	putStrLn "Start Thread Unget"
	forkIO $ ungetter chan
	threadDelay 2000000
	return ()

reader :: Chan Int -> IO ()
reader channel = do
	putStrLn "start read"
	result <- readChan channel
	threadDelay 2000000
	putStrLn $ "result " ++ show result


ungetter :: Chan Int -> IO ()
ungetter channel = do
	putStrLn "start unget"
	unGetChan channel 1337
	threadDelay 2000000
	putStrLn "end unget"

empty :: Chan Int -> IO ()
empty channel = do
	putStrLn "start isEmpty"
	result <- isEmptyChan channel
	threadDelay 2000000
	putStrLn $ "result " ++ show result