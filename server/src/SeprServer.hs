
{-# LANGUAGE OverloadedStrings, FlexibleContexts, NoMonomorphismRestriction #-}

module Main where

	import qualified Control.Concurrent.Async as Async
	import Control.Concurrent.Async (Async)
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
	import Prelude hiding (log, until)




	log = liftIO . putStrLn
	atomically = liftIO . STM.atomically
	async = liftIO . Async.async
	wait = liftIO . Async.wait

	until action = flip runContT return $ callCC $ \exit -> forever $ action exit


	data HandlerMessage = Received Message | Shutdown Bool | EndMM | NetworkerError String

	data NetworkerMessage = Send Message | RequestShutdown | ReceiverError String



	main :: IO ()
	main = do

		mmqueue <- newTQueueIO

		putStrLn "listening"

		listen "*" "1025" $ \(listener, sockaddr) -> forever $ do

			acceptFork listener $ \(conn, sockaddr) -> do

				putStrLn "accepted client"

				handlerQ <- newTQueueIO :: IO (TQueue (Either LocalMessage Message))
				networkerQ <- newTQueueIO :: IO (TQueue (Either RequestShutdown Message))

				let err msg = atomically $ writeTQueue handlerQ $ Left $ LocalError msg


				networker <- async $ do

					receiver <- async $ flip evalStateT (Partial $ runGetPartial Serial.get) $ do

						let err msg = atomically $ writeTQueue networkerQ $ ReceiverError msg

						result <- try $ until $ \exit -> do

							let errex msg = err msg >> exit ()

							currentState <- get
							case currentState of
								Fail errmsg _ -> errex $ "parse error: " ++ errmsg
								Partial parse -> do
									dat <- TCP.recv conn 1400
									case dat of
										Nothing -> errex "Socket closed"
										Just dat' -> do
											put $ parse dat'
								Done result remainder -> do
									atomically $ writeTQueue handlerQ $ Right result
									put $ runGetPartial Serial.get remainder

						case result of
							Left ex -> err $ "receiver: exception: " ++ show ex
							otherwise -> return ()

					result <- try $ until $ \exit -> do
						msg <- atomically $ readTQueue senderQ
						case msg of
							Left _ -> do
								cancel receiver
								try $ wait receiver
								atomically $ writeTQueue handlerQ $ Left $ Shutdown True
								exit ()
							Right msg -> do
								TCP.send conn $ runPut $ Serial.put msg
							_ -> do
								err: "sender: received strange message"
								exit ()
					case result of
						Left ex -> err $ "receiver: exception: " ++ show ex
						otherwise -> return ()


							

				let
					send msg = atomically $ writeTQueue senderQ $ Right $ ServerClient msg
					pass msg = atomically $ writeTQueue senderQ $ Right $ ClientClient msg

					recv = atomically $ readTQueue handlerQ

					close = do
						atomically $ writeTQueue toSend $ Left "mclose called"
						log "mclose called, closing socket"
						closeSock conn

					error msg = do
						log msg
						send NetworkError
						close



					idle exit = forever $ do
						msg <- recv
						case msg of
							Left err -> do
								error $ "idler: error from receiver: " ++ err ++ ", exiting"
								exit Nothing
							Right (ClientServer BeginMM) -> do
								log $ "idler: sending client to matchmaking"
								send AckBeginMM
								matchmaking exit False
							Right (ClientServer _) -> do
								log $ "idler: state error"
								send $ StateError Idle
							Right _ -> do
								error $ "idler: received strange message, merroring"
								exit Nothing




					toMatchmaking exit priority = do
						stop <- newTVarIO False
						let makeMeStop = do
							atomically $ writeTVar stop True
							liftIO $ wait self
						atomically $ (if priority then unGetTQueue else writeTQueue) mmqueue (conn, makeMeStop, toMatchmaking)
						forever $ do
							shouldStop <- liftIO $ readTVarIO stop
							when shouldStop $ do
								exit True
								return ()
							msg <- recv
							case msg of
								Left err -> do
									error $ "matchmaker: error from receiver: " ++ err ++ ", merroring"
									exit False
								Right (ClientServer CancelMM) -> do
									log $ "matchmaker: client has quit matchmaking"
									send AckCancelMM
									idle
									exit False
								Right (ClientServer _) -> do
									log $ "matchmaker: state error"
									send $ StateError MM
								Right _ -> do
									error $ "matchmaker: received strange message, merroring"
									exit False

					toGame otherClientToIdle passOtherClient sendOtherClient assumeDirectControl = async $ do
						until $ \exit -> do

							msg <- recv

							case msg of
								Left err -> do
									assumeDirectControl
									sendOtherClient OpponentQuit
									otherClientToIdle
									error $ "idler: error from receiver: " ++ err ++ ", merroring"
									exit ()
								Right (ClientServer RequestQuit) -> do
									log $ "received quit request, quitting game"
									log $ "ASSUMING DIRECT CONTROL (lol)"
									assumeDirectControl
									sendOtherClient OpponentQuit
									otherClientToIdle
									send AckRequestQuit
									lift $ toIdle
									exit ()
								Right (ClientServer _) -> do
									log $ "idler: state error"
									send $ StateError Game
								Right (ClientClient m) -> do
									passOtherClient m
								Right _ -> do
									assumeDirectControl
									sendOtherClient OpponentQuit
									otherClientToIdle
									error $ "idler: received strange message, merroring"
									exit ()
						log "idle process ended"



				toIdle
				putStrLn "started idle process for client"
				wait sender
				wait receiver
				return ()

		
		putStrLn "done listening"


