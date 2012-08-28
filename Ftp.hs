{-# LANGUAGE ViewPatterns #-}
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as S
import qualified Network.Socket as NS
import Network.FTP.Server
import Network.FTP.Backend.FileSystem

main :: IO ()
main = do
    [S.pack -> base] <- getArgs
    let serverConf = ServerSettings 8000 HostAny
    runFilesystemBackend (defaultFilesystemState base) $
        runTCPServer serverConf ftpServer
