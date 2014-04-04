
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, ScopedTypeVariables #-}

module Protocol where


	import Data.Serialize
	import Data.Serialize.Get
	import Data.Serialize.Put
	import Control.Monad
	import Control.Applicative
	import Data.ByteString.Lazy hiding (empty)
	import Prelude hiding (length, until)
	import Network.Simple.TCP hiding (recv, send)
	import qualified Network.Simple.TCP as TCP
	import Control.Monad.Catch
	import Control.Concurrent.Async.Lifted
	import qualified Control.Monad.State.Lazy as StateM
	import Control.Monad.State.Lazy hiding (get, put, State)
	import Control.Monad.Cont
	import Control.Concurrent.STM.Lifted
	import Data.Typeable
	import Control.Monad.Trans.Control
	import Control.Monad.STM (throwSTM)


	data State = Game | MM | Idle

	data Message = ServerClient ServerClient | ClientServer ClientServer | ClientClient ClientClient

	data RecvMessage = ToUs ClientServer | ToThem ClientClient

	data SentMessage = FromUs ServerClient | FromThem ClientClient

	data ServerClient = AckBeginMM | AckCancelMM | FoundGame | OpponentQuit | AckRequestQuit | StateError State | NetworkError

	data ClientServer = BeginMM | CancelMM | RequestQuit

	data ClientClient = Object ByteString


	instance Serialize State where
		put Game = putWord8 0
		put MM = putWord8 1
		put Idle = putWord8 2
		get = do
			state <- getWord8
			case state of
				0 -> return Game
				1 -> return MM
				2 -> return Idle
				o -> label ("Did not recognise the State indicated by byte " ++ show o) empty

	instance Serialize Message where
		put (ServerClient m) = do
			putWord8 0
			put m
		put (ClientServer m) = do
			putWord8 1
			put m
		put (ClientClient m) = do
			putWord8 2
			put m
		get = do
			msgType <- getWord8
			case msgType of
				0 -> ServerClient <$> get
				1 -> ClientServer <$> get
				2 -> ClientClient <$> get
				o -> label ("Did not recognise the Message type indicated by byte " ++ show o) empty

	instance Serialize SentMessage where
		put (FromUs m) = do
			putWord8 0
			put m
		put (FromThem m) = do
			putWord8 2
			put m
		get = do
			msgType <- getWord8
			case msgType of
				0 -> FromUs <$> get
				2 -> FromThem <$> get
				o -> label ("Did not recognise the SentMessage type indicated by byte " ++ show o) empty

	instance Serialize RecvMessage where
		put (ToUs m) = do
			putWord8 1
			put m
		put (ToThem m) = do
			putWord8 2
			put m
		get = do
			msgType <- getWord8
			case msgType of
				1 -> ToUs <$> get
				2 -> ToThem <$> get
				o -> label ("Did not recognise the ReceivedMessage type indicated by byte " ++ show o) empty

	instance Serialize ServerClient where
		put AckBeginMM = putWord8 0
		put AckCancelMM = putWord8 1
		put FoundGame = putWord8 2
		put OpponentQuit = putWord8 3
		put AckRequestQuit = putWord8 4
		put (StateError state) = do
			putWord8 5
			put state
		put NetworkError = putWord8 6
		get = do
			msgType <- getWord8
			case msgType of
				0 -> return AckBeginMM
				1 -> return AckCancelMM
				2 -> return FoundGame
				3 -> return OpponentQuit
				4 -> return AckRequestQuit
				5 -> StateError <$> get
				6 -> return NetworkError
				o -> label ("Did not recognise the ServerClient Message type indicated by byte " ++ show o) empty

	instance Serialize ClientServer where
		put BeginMM = putWord8 0
		put CancelMM = putWord8 1
		put RequestQuit = putWord8 2
		get = do
			msgType <- getWord8
			case msgType of
				0 -> return BeginMM
				1 -> return CancelMM
				2 -> return RequestQuit
				o -> label ("Did not recognise the ClientServer Message type indicated by byte " ++ show o) empty

	instance Serialize ClientClient where
		put (Object dat) = do
			putWord32be $ fromIntegral $ length dat
			put dat
		get = do
			len <- getWord32be
			dat <- getLazyByteString $ fromIntegral len
			return $ Object dat



	until action = flip runContT return $ untilCC action
	untilCC action = callCC $ \break -> forever $ action break


	tailCC action exit = do
		result <- callCC $ \exit' -> action (\v -> exit' (Left v) >> return (Left v)) (exit' . Right)
		case result of
			Left val -> exit val
			Right cont -> cont exit

	data LocalMessage = Send SentMessage | RequestClose | LocalError String
	data NetworkMessage = Closed | Error String | Received RecvMessage

	data SocketException = SocketClosed | SocketError String deriving (Show, Typeable)
	instance Exception SocketException

	data SeprSocket = SeprSocket {recv :: STM (Either SocketException RecvMessage), send :: ServerClient -> STM (), pass :: ClientClient -> STM (), close :: STM ()}


	createSeprSocket :: (MonadBaseControl IO m, MonadIO m, MonadCatch m) => Socket -> m SeprSocket
	createSeprSocket sock = do
		networkMessageQ <- newTQueueIO
		localMessageQ <- newTQueueIO
		networkMessageHandler <- async $ do

			

			result <- try $ flip evalStateT (Partial $ runGetPartial get) $ until $ \exit -> do

				currentState <- StateM.get
				case currentState of
					Fail errmsg _ -> exit $ Just $ "parse error: " ++ errmsg
					Partial parse -> do
						dat <- TCP.recv sock 1400
						case dat of
							Nothing -> exit $ Just $ "socket closed"
							Just dat' -> StateM.put $ parse dat'
					Done result remainder -> do
						atomically $ writeTQueue networkMessageQ $ Received result
						StateM.put $ runGetPartial get remainder

			let err msg = atomically $ writeTQueue localMessageQ $ LocalError msg

			uninterruptibleMask_ $ case result of
				Left (ex :: SomeException) -> err $ "receiver: exception: " ++ show ex
				Right (Just msg) -> err $ "receiver: error: " ++ msg
				Right Nothing -> return ()

		localMessageHandler <- async $ do

			result <- try $ until $ \exit -> do
				msg <- atomically $ readTQueue localMessageQ
				case msg of
					LocalError msg -> exit $ Just msg
					RequestClose -> exit Nothing
					Send msg -> do
						TCP.send sock $ runPut $ put msg

			let stopTasks = cancel networkMessageHandler

			let err msg = void $ do
				stopTasks
				atomically $ writeTQueue networkMessageQ $ Error msg

			uninterruptibleMask_ $ case result of
				Left (ex :: SomeException) -> err $ "receiver: exception: " ++ show ex
				Right (Just msg) -> err $ "receiver: error: " ++ msg
				Right Nothing -> do
					stopTasks
					atomically $ writeTQueue networkMessageQ Closed


		let
			recv = do
				msg <- readTQueue networkMessageQ
				case msg of
					Closed -> do
						unGetTQueue networkMessageQ Closed
						return $ Left SocketClosed
					Error msg -> do
						unGetTQueue networkMessageQ $ Error msg
						return $ Left $ SocketError msg
					Received msg -> return $ Right msg
			sendAny msg = do
				writeTQueue localMessageQ $ Send msg
			{-
				receivedMsg <- tryPeekTQueue networkMessageQ
				case receivedMsg of
					Just Closed -> throwSTM SocketClosed
					Just (Error msg) -> throwSTM $ SocketError msg
					otherwise -> writeTQueue localMessageQ $ Send msg
			-}
			close = do
				writeTQueue localMessageQ $ RequestClose

		return $ SeprSocket recv (sendAny . FromUs) (sendAny . FromThem) close








