
{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module Main where

	import Control.Monad.Catch
	import Control.Concurrent.Async.Lifted
	import Control.Concurrent.STM.Lifted
	import Control.Monad.Trans.Control
	import Control.Monad
	import Control.Applicative
	import Protocol.Server
	import Control.Monad.Cont
	import Control.Monad.State.Lazy
	import Data.Monoid
	import Data.Time
	import Prelude hiding (log, until)
	import Network.Simple.TCP (serve)


	(<||>) :: (Alternative f) => f a -> f b -> f (Either a b)

	a <||> b = (Left <$> a) <|> (Right <$> b)

	log :: (MonadState String m, MonadIO m, Show UTCTime) => String -> m ()
	log msg = do
		t <- liftIO getCurrentTime
		start <- get
		put $ start ++ show t ++ ": " ++ msg ++ "\n"

	data MMClient = MMClient {beginGame :: MMClient -> STM (),  getMessage :: STM ClientMessage}
	data ClientMessage = Ready | Quit | Pass ClientClient




	main :: IO ()
	main = do

		mmqueue <- newTQueueIO

		putStrLn "listening"

		async $ forever $ do
			client1 <- atomically $ readTQueue mmqueue
			client2 <- atomically $ readTQueue mmqueue
			atomically $ beginGame client1 client2
			atomically $ beginGame client2 client1


		serve "*" "1025" $ \(sock, address) -> do

			SeprServerSocket recv send pass close <- createServerSocket <$> createSeprSocket sock

			result <- try $ flip runStateT "begin log:\n" $ flip runContT return $ do
				log $ "accepted client with address " ++ show address




				let
					idle :: (() -> ContT () (StateT String IO) b) -> ContT () (StateT String IO) b
					matchmaking :: Bool -> (() -> ContT () (StateT String IO) b) -> ContT () (StateT String IO) b
					game :: MMClient -> (ClientMessage -> STM ()) -> (() -> ContT () (StateT String IO) b) -> ContT () (StateT String IO) b

					idle = tailCC $ \exit call -> do
						error <- callCC $ \err -> forever $ do
							msg <- atomically $ recv
							case msg of
								Right msg -> case msg of
									ToUs BeginMM -> do
										log $ "idle: sending client to matchmaking"
										atomically $ send AckBeginMM
										call $ matchmaking False
									otherwise -> do
										log $ "idle: state error"
										atomically $ send $ StateError Idle
								Left ex -> err $ show ex

						do
							log $ "idle: fatal error: " ++ error
							exit ()


					matchmaking priority = tailCC $ \exit call -> do
						messages <- newTQueueIO
						otherClientVar <- newEmptyTMVarIO

						let tellOtherClient msg = writeTQueue messages msg

						let
							beginGame otherClient = putTMVar otherClientVar otherClient
							getMessage = readTQueue messages

						atomically $ (if priority then unGetTQueue else writeTQueue) mmqueue $ MMClient beginGame getMessage

						error <- callCC $ \err -> forever $ do
							msg <- atomically $ recv <||> takeTMVar otherClientVar
							case msg of
								Left fromOurClient -> case fromOurClient of
									Left ex -> err $ show ex
									Right msg -> case msg of
										ToUs CancelMM -> do
											log $ "mm: sending client back to idle"
											atomically $ send AckCancelMM
											atomically $ tellOtherClient Quit
											call idle
										otherwise -> do
											log $ "mm: state error"
											atomically $ send $ StateError MM
								Right gotOtherClient -> do
									log $ "mm: got game"
									call $ game gotOtherClient tellOtherClient


						do
							log $ "mm: fatal error: " ++ error
							log $ "mm: quitting mm"
							atomically $ tellOtherClient Quit
							exit ()

					game otherClient tellOtherClient = tailCC $ \exit call -> do

						error <- callCC $ \err -> do

							atomically $ tellOtherClient Ready
							msg <- atomically $ getMessage otherClient

							callCC $ \continue -> case msg of
								Ready -> continue ()
								otherwise -> do
									log $ "game: bad game, rejoining mm"
									atomically $ tellOtherClient Quit
									call $ matchmaking True

							log $ "game: game ok, sending foundgame to client"
							atomically $ send FoundGame

							forever $ do
								msg <- atomically $ recv <||> getMessage otherClient 
								case msg of
									Right fromOtherClient -> case fromOtherClient of
										Quit -> do
											log $ "game: other client quit"
											atomically $ tellOtherClient Quit
											atomically $ send OpponentQuit
											call $ idle
										Ready -> return ()
										Pass msg -> atomically $ pass msg
									Left fromOurClient -> case fromOurClient of
										Left ex -> err $ show ex
										Right msg -> case msg of
											ToUs RequestQuit -> do
												atomically $ tellOtherClient Quit
												atomically $ send AckRequestQuit
												call $ idle
											ToThem msg -> do
												atomically $ tellOtherClient $ Pass msg
											otherwise -> do
												log $ "game: state error"
												atomically $ send $ StateError Game

						do
							log $ "game: fatal error: " ++ error
							log $ "game: quitting game"
							atomically $ tellOtherClient Quit
							exit ()



				callCC idle

			case result of
				Left (ex :: SomeException) -> putStrLn $ "client exited with exception: " ++ show ex
				Right ((), log) -> putStrLn $ "client exited with the following log: \n" ++ log

