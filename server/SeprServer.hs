
{-# LANGUAGE OverloadedStrings #-}

module Main where

	import Control.Concurrent.Async
	import Network.Socket.ByteString.Lazy
	import Network.Socket hiding (send, sendTo, recv, recvFrom)
	import Prelude hiding (getContents, catch)
	import Control.Monad
	import Control.Concurrent.STM.TQueue
	import Control.Exception
	import Control.Monad.STM

	bounce x = async $ forever $ do
		recv x 4096
		sendAll x "\r\nError: Not in game!\r\n"

	main :: IO ()
	main = do
		notInGame <- newTQueueIO
		sock <- socket AF_INET Stream 0
		setSocketOption sock ReuseAddr 1
		bindSocket sock (SockAddrInet 1025 iNADDR_ANY)
		listen sock 10

		async $ forever $ do
			(x, xbouncer) <- atomically $ readTQueue notInGame
			(y, ybouncer) <- atomically $ readTQueue notInGame
			cancel xbouncer
			cancel ybouncer

			let pipe x y = async $ do
				(forever $ do
					dat <- recv x 4096
					sendAll y dat)
				`catch` \(SomeException _) -> do
					sClose x
					sClose y

			pipe x y
			pipe y x

		forever $ do
			(conn, sockaddr)  <- accept sock
			bouncer <- bounce conn
			atomically $ writeTQueue notInGame (conn, bouncer)
