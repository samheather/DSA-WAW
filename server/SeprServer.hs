
module Main where

	import Control.Concurrent.Async
	import Network.Socket.ByteString.Lazy
	import Network.Socket hiding (send, sendTo, recv, recvFrom)
	import Prelude hiding (getContents, catch)
	import Control.Monad
	import Control.Concurrent.STM.TQueue
	import Control.Exception
	import Control.Monad.STM

	main :: IO ()
	main = do
		notInGame <- newTQueueIO
		sock <- socket AF_INET Stream 0
		setSocketOption sock ReuseAddr 1
		bindSocket sock (SockAddrInet 1025 iNADDR_ANY)
		listen sock 10
		async $ forever $ do
			x <- atomically $ readTQueue notInGame
			y <- atomically $ readTQueue notInGame
			async $ do
				(forever $ do
					dat <- recv x 4096
					sendAll y dat)
				`catch` \(SomeException _) -> do
					sClose x
					sClose y
			async $ do
				(forever $ do
					dat <- recv y 4096
					sendAll x dat)
				`catch` \(SomeException _) -> do
					sClose x
					sClose y
		forever $ do
			(conn, sockaddr)  <- accept sock
			atomically $ writeTQueue notInGame conn