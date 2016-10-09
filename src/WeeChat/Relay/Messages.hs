module WeeChat.Relay.Messages where

import Control.Applicative

import qualified Data.Serialize as Cereal
import Data.ByteString (ByteString)

import WeeChat.Relay.Objects
import WeeChat.Relay.Objects.Types

data Message = Message
  { messageLength :: Int
  , messageCompressed :: Bool
  , messageId' :: Str
  , messageData :: [Object]
  } deriving (Eq, Ord, Show, Read)

-- | Read 4 + 1 bytes, giving the length of the remaining message and whether
-- it is compressed.
parseHeader
  :: ByteString -- ^ Encoded message
  -> Either String (Int, Bool)
parseHeader = Cereal.runGet getHeader

getHeader :: Cereal.Get (Int, Bool)
getHeader = liftA2 (,) (fromIntegral <$> Cereal.getWord32be) Cereal.get

parseMessage :: ByteString -> Either String (Str, [Object])
parseMessage = Cereal.runGet getMessage

getMessage :: Cereal.Get (Str, [Object])
getMessage = liftA2 (,) (parse str) getMessageBody

getMessageBody :: Cereal.Get [Object]
getMessageBody = do
  b <- Cereal.isEmpty
  case b of
    True -> return []
    False -> liftA2 (:) (parse taggedObject) getMessageBody
