{-# LANGUAGE OverloadedStrings #-}
module WeeChat.Relay.Command where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (intersperse)
import Data.Bool (bool)

data Command
  = Init
      (Maybe ByteString) -- ^ Password
      (Maybe Bool) -- ^ Compression
  | Hdata -- ^ TODO
  | Info ByteString -- ^ Name
  | InfoList ByteString -- ^ TODO
  | NickList (Maybe ByteString) -- ^ Buffer
  | Input
      ByteString -- ^ Buffer
      ByteString -- ^ Data
  | Sync
      [ByteString] -- ^ Buffers
      [ByteString] -- ^ Options: buffers, upgrade, buffer, nicklist
  | DeSync
      [ByteString] -- ^ Buffers
      [ByteString] -- ^ Options: buffers, upgrade, buffer, nicklist
  | Test
  | Ping [ByteString]
  | Quit
  deriving (Eq, Ord, Show, Read)

renderCommand
  :: Maybe ByteString -- ^ Message identifier, must not start with an underscore
  -> Command
  -> ByteString
renderCommand iden cmd = case iden of
    Just iden -> BS.concat ["(", iden, ")", rendered, "\n"]
    Nothing -> BS.snoc rendered '\n'
  where
    rendered = renderCommand' cmd

renderCommand' :: Command -> ByteString
renderCommand' (Init pwd compress) =
    BS.concat $ "init " : intersperse "," options
  where
    options =
      [ BS.concat [option, "=", value]
      | Just (option, value) <- [pass <$> pwd, comp <$> compress]
      ]
    pass = (,) "password" . escapeCommas
    comp = (,) "compression" . bool "off" "zlib"
renderCommand' (Info name) = BS.append "info " name
renderCommand' (NickList buffer) =
  maybe "nicklist" (BS.append "nicklist ") buffer
renderCommand' (Input buffer dat) =
  BS.intercalate " " ["input", buffer, dat]
renderCommand' (Sync buffers options) =
  BS.intercalate " "
    [ "sync"
    , BS.intercalate "," buffers
    , BS.intercalate "," options
    ]
renderCommand' (DeSync buffers options) =
  BS.intercalate " "
    [ "desync"
    , BS.intercalate "," buffers
    , BS.intercalate "," options
    ]
renderCommand' Test = "test"
renderCommand' (Ping args) = BS.intercalate " " $ "ping" : args
renderCommand' Quit = "quit"

escapeCommas :: ByteString -> ByteString
escapeCommas = BS.intercalate "\\," . BS.split ','

