import qualified Network.Socket as NS
import Network.FTP.Server
import Network.FTP.Backend.FileSystem

main :: IO ()
main = do
    let serverConf = ServerSettings 8000 HostAny
    runFilesystemBackend defaultFilesystemState $
        runTCPServer serverConf ftpServer
