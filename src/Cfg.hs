{-# LANGUAGE OverloadedStrings #-}

module Cfg
  ( loadCfg,
    Config (..),
    Ubootp (..),
  )
where

import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Net.IPv4 (IPv4)
import qualified Net.IPv4 as IPv4
import Net.Mac (Mac)
import qualified Net.Mac as Mac
import System.Exit (ExitCode (ExitFailure), exitWith)
import Toml (TomlCodec, (.=))
import qualified Toml

dropQuotes :: Text -> Text
dropQuotes = Text.filter (/= '"')

data Config = Config
  { configUbootp :: Ubootp,
    configMapping :: Map Mac IPv4
  }

data Ubootp = Ubootp
  { ubootpMulticastAddress :: IPv4,
    ubootpMulticastPort :: Integer,
    ubootpNetmask :: IPv4,
    ubootpGateway :: IPv4,
    ubootpController :: IPv4
  }

parseIPv4 :: Text -> Either Text IPv4
parseIPv4 = convert . IPv4.decode
  where
    convert Nothing = Left "Invalid IPv4"
    convert (Just ipv4) = Right ipv4

ipv4Codec :: Toml.Key -> TomlCodec IPv4
ipv4Codec = Toml.textBy IPv4.encode parseIPv4

justMac :: Maybe Mac -> Either Toml.TomlBiMapError Mac
justMac Nothing = Left (Toml.ArbitraryError "Key not a mac address")
justMac (Just mac) = Right mac

keyMac :: Toml.TomlBiMap Toml.Key Mac
keyMac =
  Toml.BiMap
    { Toml.forward = justMac . Mac.decode . dropQuotes . Toml.prettyKey,
      Toml.backward = first (Toml.ArbitraryError . Toml.unTomlParseError) . Toml.parseKey . Mac.encode
    }

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.table ubootpCodec          "ubootp"  .= configUbootp
    <*> Toml.tableMap keyMac ipv4Codec  "mapping" .= configMapping

ubootpCodec :: TomlCodec Ubootp
ubootpCodec =
  Ubootp
    <$> ipv4Codec                 "multicastAddress"  .= ubootpMulticastAddress
    <*> Toml.diwrap (Toml.integer "multicastPort")    .= ubootpMulticastPort
    <*> ipv4Codec                 "netmask"         .= ubootpNetmask
    <*> ipv4Codec                 "gateway"         .= ubootpGateway
    <*> ipv4Codec                 "controller"      .= ubootpController

loadCfg :: IO Config
loadCfg = do
  tomlRes <- Toml.decodeFileEither configCodec "app/cfg.toml"
  case tomlRes of
    Left errs -> TIO.putStrLn (Toml.prettyTomlDecodeErrors errs) >> exitWith (ExitFailure 1)
    Right settings -> return settings
