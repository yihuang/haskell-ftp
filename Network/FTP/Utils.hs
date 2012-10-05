{-# LANGUAGE OverloadedStrings #-}
module Network.FTP.Utils where

import qualified Prelude
import BasicPrelude
import qualified Filesystem.Path as Path
import qualified Filesystem.Path.CurrentOS as Path
import qualified Data.Text.Encoding as T

-- | drop root
dropHeadingPathSeparator :: FilePath -> FilePath
dropHeadingPathSeparator p = fromMaybe p $ Path.stripPrefix "/" p

-- | encode file path.
encode :: FilePath -> ByteString
encode = T.encodeUtf8 . either id id . Path.toText

-- | decode file path.
decode :: ByteString -> FilePath
decode = Path.fromText . T.decodeUtf8
