{-# LANGUAGE CPP, OverloadedStrings #-}
module Network.FTP.Backend.Utils where

import qualified Prelude
import BasicPrelude
import qualified Filesystem.Path as Path

dropHeadingPathSeparator :: FilePath -> FilePath
dropHeadingPathSeparator p = fromMaybe p $ Path.stripPrefix "/" p
