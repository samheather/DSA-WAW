
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Main where

	import Control.Concurrent.Async
	import Network.Simple.TCP
	import Control.Monad
	import Control.Concurrent.STM.TQueue
	import Control.Exception
	import qualified Control.Monad.STM as STM
	import Protocol
	import Data.Serialize.Get
	import Data.Serialize.Put
	import Data.Serialize hiding (put, get)
	import qualified Data.Serialize as Serial
	import Control.Monad.State.Lazy
	import Control.Monad.Catch
	import Prelude hiding (log)

	log :: MonadIO m => String -> m ()
	log = liftIO . putStrLn
	atomically :: MonadIO m => STM.STM a -> m a
	atomically = liftIO . STM.atomically


	sender conn toSend = fix $ \continue -> do
		msg <- atomically $ readTQueue toSend
		case msg of
			Left err -> do
				log "sender: received error, closing socket"
				closeSock conn
			Right msg -> do
				send conn $ runPut $ Serial.put msg
				continue

	receiver conn received = flip evalStateT (Partial $ runGetPartial Serial.get) $ fix $ \continue -> do
		currentState <- get
		case currentState of
			Fail err _ -> do
				atomically $ writeTQueue received $ Left err
				log "receiver: parse error, closing socket"
				closeSock conn
			Partial parse -> do
				dat <- recv conn 1500
				case dat of
					Nothing -> do
						atomically $ writeTQueue received $ Left "Socket closed during receive"
						log "receiver: socket closed, exiting"
					Just dat' -> do
						put $ parse dat'
						continue
			Done result remainder -> do
				put $ runGetPartial Serial.get remainder
				atomically $ writeTQueue received $ Right result
				continue

	data Client = Client (TQueue (Either String Message)) (TQueue (Either String Message)) Socket

	msend (Client _ toSend _) msg = atomically $ writeTQueue toSend $ Right $ ServerClient msg
	mrecv (Client received _ _) = atomically $ readTQueue received
	mclose (Client _ toSend conn) = do
		atomically $ writeTQueue toSend $ Left "mclose called"
		log "mclose called, closing socket"
		closeSock conn
	merror client = do
		msend client $ NetworkError
		mclose client

	idle client = fix $ \continue -> do
		msg <- mrecv client
		case msg of
			Left err -> do
				log $ "idler: error from receiver: " ++ err ++ ", merroring"
				merror client
			Right (ClientServer BeginMM) -> do
				log $ "idler: sending client to matchmaking"
				-- TODO
			Right (ClientServer _) -> do
				log $ "idler: state error"
				msend client $ StateError Idle
				continue
			Right _ -> do
				log $ "idler: received strange message, merroring"
				merror client









	mfix2 action1 action2 = mfix f where
		f ~(result1, result2) = do
			result1' <- action1 result2
			result2' <- action2 result1
			return (result1', result2')


	main :: IO ()
	main = do

		matchmaking <- newTQueueIO
		listen HostAny "1025" $ \(conn, sockaddr) -> do
			received <- liftIO newTQueueIO
			toSend <- liftIO newTQueueIO
			async $ idle (Client received toSend conn)
			return ()

