module MVar where
import Control.Concurrent.STM

type MVar a = TVar (Maybe a)

newEmptyMVar :: STM (MVar a)
newEmptyMVar = do
	newTVar Nothing

takeMVar :: MVar a -> STM a
takeMVar m = do
	mv <- readTVar m
	case mv of
		Nothing -> retry
		Just v -> do
			writeTVar m Nothing
			return v

putMVar :: MVar a -> a ->  STM ()
putMVar m v = do
	mv <- readTVar m
	case mv of
		Just _ -> retry
		Nothing -> writeTVar m (Just v)

-- fuehrt die erste Transaktion (oberer Sequenzblock) aus
-- `orElse` sorgt dafür, dass ein "retry" der oberen Transaktion "gefangen" wird,
-- und anstelle der erneuten Ausführung der oberen Transaktionssequenz
-- wird dann die untere Transaktionssequenz ausgeführt.
tryTakeMVar :: MVar a -> STM (Maybe a)
tryTakeMVar m = do
		v <- takeMVar m
		return (Just v)
	`orElse`
		return Nothing
