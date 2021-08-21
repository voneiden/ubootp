{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cfg (Config (..), Ubootp (..), loadCfg)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Word (Word8)
import Net.IPv4 (IPv4)
import qualified Net.IPv4 as IPv4
import Net.Mac (Mac)
import qualified Net.Mac as Mac
import Network.Multicast (multicastReceiver, multicastSender, setTimeToLive)
import qualified Network.Socket as Socket
import Network.Socket.ByteString (recv, recvFrom, send, sendTo)
import Text.Hex (encodeHex)

sextupleToList :: (a, a, a, a, a, a) -> [a]
sextupleToList (t1, t2, t3, t4, t5, t6) = [t1, t2, t3, t4, t5, t6]

quadrupleToList :: (a, a, a, a) -> [a]
quadrupleToList (t1, t2, t3, t4) = [t1, t2, t3, t4]

packMac :: Mac -> ByteString
packMac = ByteString.pack . sextupleToList . Mac.toOctets

packIp :: IPv4 -> ByteString
packIp = ByteString.pack . quadrupleToList . IPv4.toOctets

unpackMac :: ByteString -> Maybe Mac
unpackMac = macFromWord8 . ByteString.unpack
  where
    macFromWord8 :: [Word8] -> Maybe Mac
    macFromWord8 (t1 : t2 : t3 : t4 : t5 : t6 : _) = Just (Mac.fromOctets t1 t2 t3 t4 t5 t6)
    macFromWord8 _ = Nothing

createResponse :: Config -> Mac -> IPv4 -> Either Text (ByteString, Text)
createResponse cfg mac ip =
  Right
    ( "A"
        <> packMac mac
        <> packIp ip
        <> packIp (ubootpNetmask ubootp)
        <> packIp (ubootpGateway ubootp)
        <> packIp (ubootpController ubootp),
      "Assign " <> Mac.encode mac <> " -> " <> IPv4.encode ip
    )
  where
    ubootp = configUbootp cfg

validateIp :: Config -> Mac -> Maybe IPv4 -> Either Text (ByteString, Text)
validateIp cfg mac (Just ip) = createResponse cfg mac ip
validateIp _ mac Nothing = Left ("Unknown mac address: " <> Mac.encode mac)

resolveIp :: Config -> Mac -> Either Text (ByteString, Text)
resolveIp cfg mac = validateIp cfg mac $ Map.lookup mac (configMapping cfg)

validateMac :: Config -> ByteString -> Maybe Mac -> Either Text (ByteString, Text)
validateMac cfg _ (Just mac) = resolveIp cfg mac
validateMac _ rawMac Nothing = Left ("Invalid mac address: " <> encodeHex rawMac)

parseMac :: Config -> ByteString -> Either Text (ByteString, Text)
parseMac cfg rawMac = validateMac cfg rawMac $ unpackMac rawMac

parsePacket :: Config -> ByteString -> Either Text (ByteString, Text)
parsePacket cfg packet
  | cmd == "Q" = parseMac cfg $ ByteString.drop 1 packet
  | cmd == "A" = Left "Response delivered"
  | otherwise = Left ("Ignored cmd: " <> encodeHex cmd)
  where
    cmd = ByteString.take 1 packet

serveRequests :: Config -> Socket.Socket -> IO ()
serveRequests cfg socket = do
  (packet, _) <- recvFrom socket 7
  case parsePacket cfg packet of
    Left logError -> Text.IO.putStrLn logError
    Right (responseData, logMessage) -> do
      (sendSock, addr) <- multicastSender (IPv4.encodeString multicastAddress) multicastPort
      _ <- setTimeToLive sendSock 5
      _ <- sendTo sendSock responseData addr
      Text.IO.putStrLn logMessage
  where
    multicastAddress = ubootpMulticastAddress $ configUbootp cfg
    multicastPort = fromIntegral $ ubootpMulticastPort $ configUbootp cfg :: Socket.PortNumber

foreverServeBroadcasts :: Config -> IO ()
foreverServeBroadcasts cfg = do
  sock <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
  Socket.setSocketOption sock Socket.Broadcast 1
  Socket.setSocketOption sock Socket.ReuseAddr 1
  Text.IO.putStrLn "Binding broadcast socket to 255.255.255.255"
  Socket.bind sock (Socket.SockAddrInet 42069 (Socket.tupleToHostAddress (255, 255, 255, 255)))

  forever $ do
    packet <- recv sock 7
    Text.IO.putStrLn "Processing broadcast"
    case parsePacket cfg packet of
      Left logError -> Text.IO.putStrLn logError
      Right (responseData, logMessage) -> do
        _ <- send sock responseData
        Text.IO.putStrLn logMessage

main :: IO ()
main = do
  putStrLn "Loading config"
  cfg <- loadCfg
  let multicastAddress = ubootpMulticastAddress $ configUbootp cfg
  let multicastPort = fromIntegral $ ubootpMulticastPort $ configUbootp cfg :: Socket.PortNumber

  Text.IO.putStrLn ("Binding socket to " <> IPv4.encode multicastAddress <> ":" <> Text.pack (show multicastPort))
  socket <- multicastReceiver (IPv4.encodeString multicastAddress) multicastPort
  putStrLn "Serving requets forever"
  _ <- forkIO $ foreverServeBroadcasts cfg
  forever $ serveRequests cfg socket
