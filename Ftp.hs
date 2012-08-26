import Data.Conduit.Network
import qualified Network.Socket as NS
import Network.FTP.Server (ftpServer)

--myRunTCPServer :: (MonadIO m, MonadBaseControl IO m) => ServerSettings -> (SockAddr -> Application m) -> m ()
--myRunTCPServer (ServerSettings port host) app = control $ \run -> bracket
--    (liftIO $ bindPort port host)
--    (liftIO . NS.sClose)
--    (run . forever . serve)
--  where
--    serve lsocket = do
--        (socket, _addr) <- liftIO $ NS.accept lsocket
--        let src = sourceSocket socket
--            sink = sinkSocket socket
--            app' run = run (app _addr src sink) >> return ()
--            appClose run = app' run `finally` NS.sClose socket
--        control $ \run -> forkIO (appClose run) >> run (return ())

main :: IO ()
main = do
    let serverConf = ServerSettings 8000 HostAny
    runTCPServer serverConf ftpServer
