
{-# LANGUAGE OverloadedStrings #-}

module Main where

	import Control.Concurrent.Async
	import Network.Socket.ByteString.Lazy
	import Network.Socket hiding (send, sendTo, recv, recvFrom)
	import Prelude hiding (getContents, catch, length)
	import Data.ByteString.Lazy (length)
	import Control.Monad
	import Control.Concurrent.STM.TQueue
	import Control.Exception
	import Control.Monad.STM
	import Data.Function
	import Control.Applicative
	import Control.Monad.Fix


	forever' f = forever $ fix f


	bounce x = async $ forever $ do

		dat <- recv x 1500
		when (length dat < 1) $ throwIO $ ErrorCall "Socket failure"
		sendAll x "Error: Not in game!\r\n"


	ready x = do

		connected <- isConnected x
		readable <- isReadable x
		writeable <- isWritable x
		return $ connected && readable && writeable


	whenM pred action = do

		b <- pred
		when b action


	unlessM pred = whenM $ not <$> pred


	ifM pred whenTrue whenFalse = do

		b <- pred
		if b

			then whenTrue
			else whenFalse


	mfix2 action1 action2 = mfix $ \x -> do

		let result1 = fst x
		let result2 = snd x
		result1' <- action1 result2
		result2' <- action2 result1
		return (result1', result2')



	main :: IO ()
	main = do

		notInGame <- newTQueueIO
		sock <- socket AF_INET Stream 0
		setSocketOption sock ReuseAddr 1
		bindSocket sock (SockAddrInet 1025 iNADDR_ANY)
		listen sock 32
		async $ forever' $ \continue -> do

			(x, bouncerx) <- atomically $ readTQueue notInGame
			whenM (not <$> ready x) $ do

				sClose x
				cancel bouncerx
				void continue

			fix $ \continue -> do

				(y, bouncery) <- atomically $ readTQueue notInGame
				cancel bouncery
				whenM (not <$> ready y) $ do

					sClose y
					void continue

				cancel bouncerx
				let pipe x y other = async $ forever $ do

					let cleanup x y other = do

						cancel other
						sClose x
						bouncer <- bounce y
						atomically $ writeTQueue notInGame (y, bouncer)
						throwIO $ ErrorCall "Socket failure"

					dat <- do

							dat <- recv x 1500
							when (length dat < 1) $ throwIO $ ErrorCall "Socket failure"
							return dat

						`catches` [
						Handler (\ThreadKilled -> throwIO $ ErrorCall "Thread failure"),
						Handler (\(SomeException _) -> cleanup x y other)]

					sendAll y dat

						`catches` [
						Handler (\ThreadKilled -> throwIO $ ErrorCall "Thread failure"),
						Handler (\(SomeException _) -> cleanup y x other)]

				void $ mfix2 (pipe x y) (pipe y x)

		forever $ do

			(conn, sockaddr)  <- accept sock
			bouncer <- bounce conn
			atomically $ writeTQueue notInGame (conn, bouncer)
