
module Protocol where

	import Data.ByteString
	import Data.Binary
	import Data.Binary.Get
	import Data.Binary.Put
	import Data.Int
	import Data.Text.Encoding
	import Data.Text hiding (length, pack, unpack)
	import Prelude hiding (length)
	import Control.Monad
	import Control.Applicative


	newtype Object = ToObject ByteString
	data Error = Fatal Text | Warning Text
	data Game = Begin | End | Quit | Object Object | Matchmaking | GameError Error
	data Message = Nop | Hello | AckHello | Game Game | NetworkError Error


	getFixedLengthString = do
		len <- getWord32be
		dat <- pack <$> (sequence $ do
			[1..len]
			return getWord8)
		return $ dat

	putFixedLengthString dat = do
		putWord32be $ fromIntegral $ length dat
		put $ unpack dat


	instance Binary Object where
		put (ToObject dat) = putFixedLengthString dat
		get = ToObject <$> getFixedLengthString

	instance Binary Error where
		put (Fatal dat) = do
			putWord8 0
			putFixedLengthString $ encodeUtf16BE dat
		put (Warning dat) = do
			putWord8 1
			putFixedLengthString $ encodeUtf16BE dat
		get = do
			errorType <- getWord8
			case errorType of
				0 -> Fatal . decodeUtf16BE <$> getFixedLengthString
				1 -> Warning . decodeUtf16BE <$> getFixedLengthString

	instance Binary Game where
		put Begin = putWord8 0
		put End = putWord8 1
		put Quit = putWord8 2
		put (Object o) = do
			putWord8 3
			put o
		put Matchmaking = putWord8 4
		put (GameError e) = do
			putWord8 5
			put e
		get = do
			gameType <- getWord8
			case gameType of
				0 -> return Begin
				1 -> return End
				2 -> return Quit
				3 -> Object <$> get
				4 -> return Matchmaking
				5 -> GameError <$> get

	instance Binary Message where
		put Nop = putWord8 0
		put Hello = putWord8 1
		put AckHello = putWord8 2
		put (Game g) = do
			putWord8 3
			put g
		put (NetworkError e) = do
			putWord8 4
			put e
		get = do
			messageType <- getWord8
			case messageType of
				0 -> return Nop
				1 -> return Hello
				2 -> return AckHello
				3 -> Game <$> get
				4 -> NetworkError <$> get