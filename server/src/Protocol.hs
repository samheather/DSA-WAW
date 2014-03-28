

module Protocol where


	import Data.Serialize
	import Data.Serialize.Get
	import Data.Serialize.Put
	import Control.Monad
	import Control.Applicative
	import Data.ByteString.Lazy hiding (empty)
	import Prelude hiding (length)


	data State = Game | MM | Idle

	data Message = ServerClient ServerClient | ClientServer ClientServer | ClientClient ClientClient

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


	createSocket :: S