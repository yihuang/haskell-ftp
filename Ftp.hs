import System.Environment (getArgs)
import Filesystem.Path.CurrentOS
import Network.FTP.Server
import Network.FTP.Backend.FileSystem

main :: IO ()
main = do
    [d] <- getArgs
    let serverConf = ServerSettings 8000 HostAny
    runFSBackend (defaultFSState (decodeString d)) $
        runTCPServer serverConf ftpServer
