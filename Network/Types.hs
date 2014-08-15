module Network.Types where

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Types
-- Copyright   :  (c) Commiters
-- License     :  The same as `nmcli` - http://manpages.ubuntu.com/manpages/maverick/man1/nmcli.1.html
--
-- Maintainer  :  massyl, ardumont
-- Stability   :  experimental
-- Portability :  unportable
-- Dependency  :  nmcli (network-manager package in debian-based platform - http://www.gnome.org/projects/NetworkManager/)
--
-- Definition types.
--
-----------------------------------------------------------------------------

import           Control.Monad.Error
import           Control.Monad.Writer (WriterT)

type WifiMonad w a = WriterT w IO a
type SSID   = String
type Signal = Integer
type Wifi   = (SSID, Signal)
type Log    = String
type Output = String

-- | A CLI command to connect or scan wifi
type CLICommand = String

-- | A command is either to scan wifi or to connect to one.
data Command = Scan { scan :: CLICommand }
             | Connect { connect :: SSID -> CLICommand }

instance Show Command where
  show (Scan _)    = "Scan wifi"
  show (Connect _) = "Connect to an elected Wifi..."

-- ####### Error

data CommandError = BadCommand String
                  | NoWifiAvailable
                  | ScanWifiError
                  | KnownWifiError
                  | ConnectionError String
                  | Default String
                  deriving Eq

instance Show CommandError where
  show NoWifiAvailable            = "No known wifi available!"
  show ScanWifiError              = "Scan wifi error."
  show KnownWifiError             = "List known wifi error."
  show (BadCommand cmd)           = "'" ++ cmd ++ "' is not a valid command."
  show (ConnectionError wifiSSID) = "Error during connection to '" ++ wifiSSID ++ "'."
  show (Default msg)              = msg

instance Error CommandError where
  noMsg = Default "An error has occured."
  strMsg = Default

type ThrowsError = Either CommandError
