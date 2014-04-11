
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, ScopedTypeVariables, LambdaCase #-}

module Protocol.Client (module Protocol, module Protocol.Client) where

	import Protocol hiding (recv, send, close)
	import Control.Concurrent.STM.Lifted
	import Control.Monad

	data SeprClientSocket = SeprClientSocket {recv :: STM (Either SocketException RecvMessage), send :: ClientServer -> STM (), pass :: ClientClient -> STM (), close :: STM ()}

	data RecvMessage = FromServer ServerClient | FromOpponent ClientClient deriving (Read, Show)

	data SentMessage = ToServer ClientServer | ToOpponent ClientClient deriving (Read, Show)


	createClientSocket :: SeprSocket -> SeprClientSocket
	createClientSocket (SeprSocket recv send close) = SeprClientSocket recv' send' pass' close' where
		recv' = do
			msg <- recv
			return $ join $ flip fmap msg $ \case
				ServerClient m -> Right $ FromServer m
				ClientClient m -> Right $ FromOpponent m
				otherwise -> Left $ SocketError "Bad message type"
		send' = send . ClientServer
		pass' = send . ClientClient
		close' = close