{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module WeeChat.Relay.Objects.Types where

import Data.Char
import Data.Int
import Data.Word

import Data.ByteString (ByteString)
import Data.Codec
import Data.Time (UTCTime)
import Data.Vector (Vector)

data ObjectType
  = CHR | INT | LON | STR | BUF | PTR | TIM | HTB | HDA | INF | INL | ARR
  deriving (Enum, Eq, Ord, Show, Read)

data Object
  = Chr Word8
  | Int Int32
  | Lon Integer
  | Str Str
  | Buf Buffer
  | Ptr Pointer
  | Tim UTCTime
  | Htb Htable
  | Hda Hdata
  | Inf Info
  | Inl InfoL
  | Arr [Object]
  deriving (Eq, Ord, Show, Read)

pattern Chr' :: Char -> Object
pattern Chr' c <- Chr (chr . fromIntegral -> c) where
  Chr' = Chr . fromIntegral . ord

pattern Int' :: Int -> Object
pattern Int' n <- Int (fromIntegral -> n) where
  Int' n = Int (fromIntegral n)

pattern ByteString :: ByteString -> Object
pattern ByteString s = Str (Just s)

type Str = Maybe ByteString

type Buffer = Maybe ByteString

newtype Pointer = Pointer { unPointer :: ByteString }
  deriving (Eq, Ord, Show, Read)

data Hdata = Hdata
  { hdaHPath :: HPath
  , hdaKeys :: [(ByteString, ObjectType)] -- ^ List of @keyName:type@ pairs.
  , hdaItems :: [HdataItem]
  } deriving (Eq, Ord, Show, Read)

type HPath = [ByteString]

data HdataItem = HdataItem
  { hdaPPath :: PPath
  , hdaValues :: [Object]
  } deriving (Eq, Ord, Show, Read)

type PPath = [Pointer]

type Identifier = ByteString

type Htable = [(Object, Object)]

type Info = (ByteString, Str)

type Name = ByteString

data InfoL = InfoL
  { infoLName :: Name
  , infoLItems :: [Item]
  } deriving (Eq, Ord, Show, Read)

type Item = [(Name, Object)]
