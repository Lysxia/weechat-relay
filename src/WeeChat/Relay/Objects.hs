{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
module WeeChat.Relay.Objects where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Int
import Data.Maybe
import Data.Word
import Prelude hiding (putStr)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Serialize (Get, PutM)
import Data.Codec
  (Codec, ($>>), (>>>), (>-<), f_1, f_2, mapCodec, mapCodecM, (=.))
import qualified Data.Codec as Codec
import Data.Bytes.Codec (BinaryCodec)
import qualified Data.Bytes.Codec as Bytes
import qualified Data.Bytes.Get as Bytes
import qualified Data.Bytes.Put as Bytes

import Data.Time (UTCTime)
import qualified Data.Time.Clock.POSIX as Time

import WeeChat.Relay.Objects.Types

isNull :: Pointer -> Bool
isNull (Pointer bs) = BS.null bs

nullPtr :: Pointer
nullPtr = Pointer ""

-- | Three-character identifier
objectType :: BinaryCodec ObjectType
objectType = mapCodec parseType unparseType (Bytes.byteString 3)

parseType :: ByteString -> ObjectType
parseType "chr" = CHR
parseType "int" = INT
parseType "lon" = LON
parseType "str" = STR
parseType "buf" = BUF
parseType "ptr" = PTR
parseType "tim" = TIM
parseType "htb" = HTB
parseType "hda" = HDA
parseType "inf" = INF
parseType "inl" = INL
parseType "arr" = ARR
parseType x = error $ "Unknown type A " ++ show x

unparseType :: ObjectType -> ByteString
unparseType CHR = "chr"
unparseType INT = "int"
unparseType LON = "lon"
unparseType STR = "str"
unparseType BUF = "buf"
unparseType PTR = "ptr"
unparseType TIM = "tim"
unparseType HTB = "htb"
unparseType HDA = "hda"
unparseType INF = "inf"
unparseType INL = "inl"
unparseType ARR = "arr"

-- | The three-character identifier for the type of the given object.
typeOf :: Object -> ObjectType
typeOf (Chr _) = CHR
typeOf (Int _) = INT
typeOf (Lon _) = LON
typeOf (Str _) = STR
typeOf (Buf _) = BUF
typeOf (Ptr _) = PTR
typeOf (Tim _) = TIM
typeOf (Htb _) = HTB
typeOf (Hda _) = HDA
typeOf (Inf _) = INF
typeOf (Inl _) = INL
typeOf (Arr _) = ARR

-- | Codec for an object of the given type.
object :: ObjectType -> BinaryCodec Object
object typ = Codec.codec (getObject typ) putObject

-- | Get component of 'object'.
getObject :: Bytes.MonadGet get => ObjectType -> get Object
getObject CHR = Chr <$> parse chr
getObject INT = Int <$> parse int
getObject LON = Lon <$> parse lon
getObject STR = Str <$> parse str
getObject BUF = Buf <$> parse str
getObject PTR = Ptr <$> parse ptr
getObject TIM = Tim <$> parse tim
getObject HTB = Htb <$> parse htb
getObject HDA = Hda <$> parse hda
getObject INF = Inf <$> parse inf
getObject INL = Inl <$> parse inl
getObject ARR = Arr <$> parse arr

-- | Put component of 'object'.
putObject :: Bytes.MonadPut put => Object -> put ()
putObject (Chr c) = produce chr c
putObject (Int n) = produce int n
putObject (Lon n) = produce lon n
putObject (Str s) = produce str s
putObject (Buf b) = produce str b
putObject (Ptr p) = produce ptr p
putObject (Tim t) = produce tim t
putObject (Htb h) = produce htb h
putObject (Hda h) = produce hda h
putObject (Inf i) = produce inf i
putObject (Inl i) = produce inl i
putObject (Arr a) = produce arr a

-- | Codec for an object encoded as @type,object@.
taggedObject :: BinaryCodec Object
taggedObject = typed id id

chr :: BinaryCodec Word8
chr = Bytes.word8

-- | The WeeChat Relay spec represents "int" as a 4-byte value.
int :: BinaryCodec Int32
int = mapCodec fromIntegral fromIntegral Bytes.word32be

-- | A convenient alternative to 'int32'.
int' :: BinaryCodec Int
int' = mapCodec fromIntegral fromIntegral Bytes.word32be

lon :: BinaryCodec Integer
lon = mapCodecM strToLon lonToStr $ countedBS Bytes.word8

strToLon :: Monad m => ByteString -> m Integer
strToLon bs = case BS.readInteger bs of
  Just (n, left) | BS.null left -> return n
  _ -> fail "lon: invalid Integer"

lonToStr :: Monad m => Integer -> m ByteString
lonToStr = return . BS.pack . show

str :: BinaryCodec (Maybe ByteString)
str = bindCodec Bytes.word32be codec len
  where
    -- (-1 :: Word32) /= (-1 :: Int) on 64 bit systems
    codec (-1) = pure Nothing
    codec n = mapCodec Just fromJust . Bytes.byteString . fromIntegral $ n
    len = maybe (-1) (fromIntegral . BS.length)

str' :: BinaryCodec ByteString
str' = mapCodec fromJust Just str

ptr :: BinaryCodec Pointer
ptr = mapCodec Pointer unPointer $ countedBS Bytes.word8

countedBS :: Integral a => BinaryCodec a -> BinaryCodec ByteString
countedBS codecLength = bindCodec codecLength' Bytes.byteString BS.length
  where
    codecLength' = mapCodec fromIntegral fromIntegral codecLength

tim :: BinaryCodec UTCTime
tim = mapCodec
  (Time.posixSecondsToUTCTime . fromIntegral)
  (truncate . Time.utcTimeToPOSIXSeconds)
  lon

htb :: BinaryCodec [(Object, Object)]
htb = bindCodec codecKV codecBody kvOf
  where
    codecKV = objectType >| objectType
    codecBody (k, v) = counted (object k >| object v)
    kvOf [] = (INT, INT)
    kvOf ((k, v) : _) = (typeOf k, typeOf v)

hda :: BinaryCodec Hdata
hda = bindCodec (hPath >| hKeyValues) codec metaOf
  where
    codec (path, kvs) = mapCodec (Hdata path kvs) hdaItems (hItems path kvs)
    metaOf (Hdata p k _) = (p, k)

hPath :: BinaryCodec HPath
hPath = mapCodec from to str
  where
    from = maybe [] $ BS.split '/'
    to [] = Nothing
    to xs = Just . BS.intercalate "/" $ xs

hKeyValues :: BinaryCodec [(ByteString, ObjectType)]
hKeyValues = mapCodec from to str
  where
    from Nothing = []
    from (Just bs) =
      fmap ((\[k, v] -> (k, parseType v)) . BS.split ':') $ BS.split ',' bs
    to [] = Nothing
    to xs = Just .
      BS.intercalate "," . fmap (\(k, v) -> BS.concat [k, ":", unparseType v]) $ xs

hItems :: HPath -> [(ByteString, ObjectType)] -> BinaryCodec [HdataItem]
hItems hpath keys = counted (hItem hpath keys)

hItem :: HPath -> [(ByteString, ObjectType)] -> BinaryCodec HdataItem
hItem hpath keys = HdataItem
  <$> hdaPPath =. traverseCodec hpath (const ptr)
  <*> hdaValues =. traverseCodec keys (object . snd)

inf :: BinaryCodec Info
inf = str' >| str

inl :: BinaryCodec InfoL
inl = InfoL
  <$> infoLName =. str'
  <*> infoLItems =. items

items :: BinaryCodec [Item]
items = counted item

item :: BinaryCodec Item
item = counted itemValue

itemValue :: BinaryCodec (Name, Object)
itemValue = str' >| taggedObject

arr :: BinaryCodec [Object]
arr = typed counted proxy
  where
    proxy = foldr const (Int 0)

(>|)
  :: forall a b get put
  . (Applicative get, Applicative put)
  => Codec get put a -> Codec get put b -> Codec get put (a, b)
a >| b = liftA2 (,) (fst =. a) (snd =. b)

-- | Chain codecs.
bindCodec
  :: (Monad get, Applicative put)
  => Codec get put meta
  -> (meta -> Codec get put a)
  -> (a -> meta)
  -> Codec get put a
bindCodec codecMeta codecWith metaOf = Codec.Codec parse produce
  where
    parse = do
      meta <- Codec.parse codecMeta
      Codec.parse (codecWith meta)
    produce a = let meta = metaOf a in
      Codec.produce codecMeta meta *>
      Codec.produce (codecWith meta) a

traverseCodec
  :: (Applicative get, Applicative put)
  => [a] -> (a -> Codec get put b) -> Codec get put [b]
traverseCodec as codec = Codec.codec parse produce
  where
    parse = traverse (Codec.parse . codec) as
    produce = \bs -> traverse_ (\(a, b) -> Codec.produce (codec a) b) $ zip as bs

-- | Encodings with a type in the first 3 bytes.
typed
  :: (Bytes.MonadGet get, Bytes.MonadPut put)
  => (Codec get put Object -> Codec get put a)
  -> (a -> Object)
  -> Codec get put a
typed codecWith proxy =
  typeOf . proxy =. objectType >>= codecWith . object

-- | Encoding of an array with an 'Int' stored in the first 4 bytes.
counted
  :: (Bytes.MonadGet get, Bytes.MonadPut put)
  => Codec get put a
  -> Codec get put [a]
counted = countedWith replicateM length

countedWith
  :: (Bytes.MonadGet get, Bytes.MonadPut put, Traversable f)
  => (Int -> get a -> get (f a))
  -> (f a -> Int)
  -> Codec get put a
  -> Codec get put (f a)
countedWith replicateM' length' codec =
  length' =. int' >>= codecN
  where
    codecN = liftA2 Codec.Codec parse produce
    parse n = replicateM' n (Codec.parse codec)
    produce _ = mapM (Codec.produce codec)

-- | This function is specialized at PutM but does not use it.
parse :: Codec get PutM a -> get a
parse = Codec.parse

-- | This function is specialized at Get but does not use it.
produce :: Functor put => Codec Get put a -> a -> put ()
produce codec = void . Codec.produce codec

