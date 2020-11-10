{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cfg (Config (..), Ubootp (..), loadCfg)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text.IO
import Data.Word (Word8)
import Net.IPv4 (IPv4)
import qualified Net.IPv4 as IPv4
import Net.Mac (Mac)
import qualified Net.Mac as Mac
import Network.Multicast (multicastReceiver, multicastSender)
import Network.Socket (PortNumber, Socket)
import Network.Socket.ByteString (recvFrom, sendTo)

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
        <> packIp (ubootpGateway ubootp),
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
validateMac _ rawMac Nothing = Left ("Invalid mac address: " <> Text.Encoding.decodeUtf8 rawMac)

parseMac :: Config -> ByteString -> Either Text (ByteString, Text)
parseMac cfg rawMac = validateMac cfg rawMac $ unpackMac rawMac

parsePacket :: Config -> ByteString -> Either Text (ByteString, Text)
parsePacket cfg packet
  | cmd == "Q" = parseMac cfg $ ByteString.drop 1 packet
  | cmd == "A" = Left "Response delivered"
  | otherwise = Left ("Ignored cmd: " <> Text.Encoding.decodeUtf8 cmd)
  where
    cmd = ByteString.take 1 packet

serveRequests :: Config -> Socket -> IO ()
serveRequests cfg socket = do
  (packet, _) <- recvFrom socket 7
  case parsePacket cfg packet of
    Left logError -> Text.IO.putStrLn logError
    Right (responseData, logMessage) -> do
      (sendSock, addr) <- multicastSender (IPv4.encodeString unicastAddress) unicastPort
      _ <- sendTo sendSock responseData addr
      Text.IO.putStrLn logMessage
  where
    unicastAddress = ubootpUnicastAddress $ configUbootp cfg
    unicastPort = fromIntegral $ ubootpUnicastPort $ configUbootp cfg :: PortNumber

main :: IO ()
main = do
  putStrLn "Loading config"
  cfg <- loadCfg
  let unicastAddress = ubootpUnicastAddress $ configUbootp cfg
  let unicastPort = fromIntegral $ ubootpUnicastPort $ configUbootp cfg :: PortNumber

  Text.IO.putStrLn ( "Binding socket to " <> IPv4.encode unicastAddress <> ":" <> Text.pack (show unicastPort))
  socket <- multicastReceiver (IPv4.encodeString unicastAddress) unicastPort
  putStrLn "Serving requets forever"
  forever $ serveRequests cfg socket

