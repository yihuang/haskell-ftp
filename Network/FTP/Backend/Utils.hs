{-# LANGUAGE CPP #-}
module Network.FTP.Backend.Utils where

import qualified Prelude
import BasicPrelude
import qualified Data.ByteString.Char8 as S
import Filesystem.Path.CurrentOS

dropHeadingPathSeparator :: FilePath -> FilePath
dropHeadingPathSeparator = decode . drop' . encode
  where
    drop' :: ByteString -> ByteString
    drop' s =
        case S.uncons s of
#ifdef mingw32_HOST_OS
            Just ('\\', s') -> s'
#else
            Just ('/',  s') -> s'
#endif
            _               -> s

