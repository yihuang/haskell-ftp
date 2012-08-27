{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent (forkIO, threadDelay)
import System.IO (stdout)
import Data.Conduit
import Data.Conduit.Binary (sinkHandle)
import Network.FTP.Server
import Network.FTP.Backend.FileSystem

main :: IO ()
main = do
    _ <- forkIO $ do
        threadDelay 1000
        putStrLn "start client."
        let clientConf = ClientSettings 8000 "127.0.0.1"
        runTCPClient clientConf $ \fromServer toServer -> do
            yield "USER test\r\nPASS test\r\nPASV\r\n" $$ toServer
            fromServer $$ sinkHandle stdout

    let serverConf = ServerSettings 8000 "*"
    runFilesystemBackend defaultFilesystemState $
        runTCPServer serverConf ftpServer
