{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
import System.IO
import Data.Conduit
import Data.Conduit.Binary
import Network.FTP.Server (ftpServer)

main :: IO ()
main = do
    let src = yield "USER test\r\nPASS test\r\nQUIT\r\n"
        snk = sinkHandle stdout
    ftpServer src snk
