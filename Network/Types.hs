module Network.Types where

import Control.Monad.Writer (WriterT)
import Control.Monad.Error

type WifiMonad w a = WriterT w IO a
type SSID   = String
type Signal = String
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
