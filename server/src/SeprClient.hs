
{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module Main where

	import Control.Monad.Catch
	import Control.Concurrent.Async.Lifted
	import Control.Concurrent.STM.Lifted
	import Control.Monad.Trans.Control
	import Control.Monad
	import Control.Applicative
	import Protocol.Client
	import Control.Monad.Cont
	import Control.Monad.State.Lazy
	import Data.Monoid
	import Data.Time
	import Prelude hiding (log, until)
	import Network.Simple.TCP (connect)
	import System.Environment
	import Data.String
	import Control.Concurrent (killThread, myThreadId)
	import Text.Read




	main :: IO ()
	main = do
		[server, port] <- getArgs

		mutex <- newTMVarIO ()
		let lock = atomically $ void $ takeTMVar mutex
		let unlock = atomically $ void $ putTMVar mutex ()

		id <- myThreadId



		connect (fromString server) (fromString port) $ \(conn, remoteAddr) -> do
			SeprClientSocket recv send pass close <- createClientSocket <$> createSeprSocket conn

			async $ forever $ do
				msg <- atomically $ recv
				case msg of
					Left err -> do
						lock
						putStrLn $ "receive error: " ++ show err
						killThread id
					Right msg -> do
						lock
						putStrLn $ "received: " ++ show msg
						unlock


			forever $ do
				lock
				msg <- getLine
				if msg /= ""
					then case readEither msg of
						Left err -> putStrLn $ "parse error: " ++ err
						Right msg -> do
							case msg of
								ToServer msg -> atomically $ send msg
								ToOpponent msg -> atomically $ pass msg
							putStrLn "sent message"
					else return ()
				unlock



