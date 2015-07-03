module Main where
import Control.Concurrent

main = do
	startTestUnget
	threadDelay 2000000
	startTestEmpty
	threadDelay 2000000
	startTestList

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

startTestList :: IO ()
startTestList = do
	putStrLn "Starting List Test"
	chan <- newChan
	let list1 = [1..2000000]
	let list2 = [2000001..4000000]
	forkIO $ writerList chan list1
	forkIO $ writerList chan list2
	let l = list1 ++ list2
	let l2 = list2 ++ list1
	threadDelay 2000000
	content <- getChanContents chan
	let bla = take (length l) content
	print (bla == l)
	print (bla == l2)

	return ()

writerList :: Chan Int -> [Int] -> IO ()
writerList channel list = do
	writeList2Chan channel list

writer :: Chan Int -> IO ()
writer channel = do
	writeChan channel 37

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