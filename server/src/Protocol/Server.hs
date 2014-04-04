
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, ScopedTypeVariables, LambdaCase #-}

module Protocol.Server (module Protocol, module Protocol.Server) where

	import Protocol hiding (recv, send, close)
	import Control.Concurrent.STM.Lifted
	import Control.Monad

	data SeprServerSocket = SeprServerSocket {recv :: STM (Either SocketException RecvMessage), send :: ServerClient -> STM (), pass :: ClientClient -> STM (), close :: STM ()}

	data RecvMessage = ToUs ClientServer | ToThem ClientClient deriving (Read, Show)

	data SentMessage = FromUs ServerClient | FromThem ClientClient deriving (Read, Show)


	createServerSocket :: SeprSocket -> SeprServerSocket
	createServerSocket (SeprSocket recv send close) = SeprServerSocket recv' send' pass' close' where
		recv' = do
			msg <- recv
			return $ join $ flip fmap msg $ \case
				ClientServer m -> Right $ ToUs m
				ClientClient m -> Right $ ToThem m
				otherwise -> Left $ SocketError "Bad message type"
		send' = send . ServerClient
		pass' = send . ClientClient
		close' = close