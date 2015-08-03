import Control.Concurrent
import Control.Concurrent.Chan
import Data.UUID
import Data.UUID.V4
import Network.Socket
import System.IO

data Message = Message UUID String
  deriving (Show, Eq)

main = do
    channel  <- newChan
    sock <- getListeningSocket
    forkIO (listenForSocketConnections sock channel)
    forkIO (getMessagesFromStdIn channel)
    displayMessagesFrom channel stdout
  where
    forever a = a >> forever a
    getListeningSocket = do
      sock <- socket AF_INET Stream 0
      bindSocket sock (SockAddrInet 7077 iNADDR_ANY)
      listen sock 2
      return sock
    listenForSocketConnections sock channel = forever $ do
      (conn, _) <- accept sock
      hdl <- socketToHandle conn ReadWriteMode
      hSetBuffering hdl NoBuffering
      forkIO (getMessagesFromSocket hdl channel)
    getMessagesFromSocket hdl channel = do
      echoChan <- dupChan channel
      forkIO (displayMessagesFrom echoChan hdl)
      forever $ do
        line <- hGetLine hdl
        uuid <- nextRandom
        writeChan channel $ Message uuid line
    getMessagesFromStdIn channel = forever $ do
      line <- getLine
      uuid <- nextRandom
      writeChan channel $ Message uuid line
    displayMessagesFrom channel hdl = do
      msgs <- getChanContents channel
      mapM_ (\msg -> hPutStrLn hdl $ show msg ) msgs
