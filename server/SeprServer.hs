
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Main where

	import Control.Concurrent.Async
	import Network.Simple.TCP hiding (send, recv)
	import qualified Network.Simple.TCP as TCP
	import Control.Monad
	import Control.Concurrent.STM.TQueue
	import Control.Concurrent.STM.TVar
	import Control.Exception (AsyncException( ThreadKilled ))
	import qualified Control.Monad.STM as STM
	import Protocol
	import Data.Serialize.Get
	import Data.Serialize.Put
	import Data.Serialize hiding (put, get)
	import qualified Data.Serialize as Serial
	import Control.Monad.State.Lazy
	import Control.Monad.Cont
	import Control.Monad.Catch
	import Prelude hiding (log)

	log :: MonadIO m => String -> m ()
	log = liftIO . putStrLn
	atomically :: MonadIO m => STM.STM a -> m a
	atomically = liftIO . STM.atomically





	startGame client1 client2 = do
		msend client1 FoundGame
		msend client2 FoundGame
		let dogame client1 client2 = do
			msg <- mrecv client1
			case msg of
				Left err -> do
					log $ "idler: error from receiver: " ++ err ++ ", merroring"
					merror client1
				Right (ClientServer RequestQuit) -> do
					log $ "idler: client requested quit, causing quit"
					msend client2 OpponentQuit
				Right _ -> do
					log $ "idler: received strange message, merroring"
					merror client1
		return ()



	main :: IO ()
	main = do

		mmqueue <- newTQueueIO
		async $ forever $ do
			(client1, matchmaker1) <- atomically $ readTQueue mmqueue
			(client2, matchmaker2) <- atomically $ readTQueue mmqueue
			cancel matchmaker1
			ready1 <- wait matchmaker1
			cancel matchmaker2
			ready2 <- wait matchmaker2
			when (not ready1 && ready2) $ do
					matchmaker <- async $ matchmaking client2 (async $ idle client2 (atomically . writeTQueue mmqueue))
					atomically $ unGetTQueue mmqueue (client2, matchmaker)
			when (not ready2 && ready1) $ do
					matchmaker <- async $ matchmaking client1 (async $ idle client1 (atomically . writeTQueue mmqueue))
					atomically $ unGetTQueue mmqueue (client1, matchmaker)
			when (ready1 && ready2) $ startGame client1 client2


		listen HostAny "1025" $ \(conn, sockaddr) -> do
			received <- liftIO newTQueueIO
			toSend <- liftIO newTQueueIO
			async $ callCC $ \exit -> forever $ do
				msg <- atomically $ readTQueue toSend
				case msg of
					Left err -> do
						log "sender: received error, closing socket"
						closeSock conn
						exit ()
					Right msg -> do
						TCP.send conn $ runPut $ Serial.put msg
			async $ flip evalStateT (Partial $ runGetPartial Serial.get) $ callCC $ \exit -> forever $ do
				currentState <- get
				case currentState of
					Fail err _ -> do
						atomically $ writeTQueue received $ Left err
						log "receiver: parse error, closing socket"
						closeSock conn
						exit ()
					Partial parse -> do
						dat <- TCP.recv conn 1500
						case dat of
							Nothing -> do
								atomically $ writeTQueue received $ Left "Socket closed during receive"
								log "receiver: socket closed, exiting"
								exit ()
							Just dat' -> do
								put $ parse dat'
					Done result remainder -> do
						put $ runGetPartial Serial.get remainder
						atomically $ writeTQueue received $ Right result

			let send msg = atomically $ writeTQueue toSend $ Right $ ServerClient msg
			let pass msg = atomically $ writeTQueue toSend $ Right $ ClientClient msg
			let recv = atomically $ readTQueue received
			let close = do
				atomically $ writeTQueue toSend $ Left "mclose called"
				log "mclose called, closing socket"
				closeSock conn
			let error = do
				send NetworkError
				close

			let idle = callCC $ \exit -> forever $ do
				msg <- recv
				callCC $ \continue -> case msg of
					Left err -> do
						log $ "idler: error from receiver: " ++ err ++ ", merroring"
						error
						exit ()
					Right (ClientServer BeginMM) -> do
						log $ "idler: sending client to matchmaking"
						send AckBeginMM
						matchmaker <- async $ matchmaking client (async $ continue)
						toMatchmaking (client, matchmaker)
						exit ()
					Right (ClientServer _) -> do
						log $ "idler: state error"
						send $ StateError Idle
					Right _ -> do
						log $ "idler: received strange message, merroring"
						error
						exit ()

			let matchmaking = callCC $ \exit -> forever $ do
				shouldStop <- liftIO $ readTVarIO stop
				when shouldStop $ exit True
				msg <- mrecv client
				case msg of
					Left err -> do
						log $ "matchmaker: error from receiver: " ++ err ++ ", merroring"
						merror client
						exit False
					Right (ClientServer CancelMM) -> do
						log $ "matchmaker: client has quit matchmaking"
						msend client AckCancelMM
						idle
						exit False
					Right (ClientServer _) -> do
						log $ "matchmaker: state error"
						msend client $ StateError Idle
					Right _ -> do
						log $ "matchmaker: received strange message, merroring"
						merror client
						exit False
			async $ idle (Client received toSend conn) (atomically . writeTQueue mmqueue)
			return ()

