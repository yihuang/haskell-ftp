import System.Environment (getArgs)
import Filesystem.Path.CurrentOS
import qualified Network.Socket as NS
import Network.FTP.Server
import Network.FTP.Backend.FileSystem

main :: IO ()
main = do
    [base] <- getArgs
    let serverConf = ServerSettings 8000 HostAny
    runFSBackend (defaultFSState (decodeString base)) $
        runTCPServer serverConf ftpServer
