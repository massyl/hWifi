module Network.Types where
import Control.Monad.Error
import Control.Monad.Writer hiding(mapM_)

type WifiMonad w a = WriterT w IO a
type SSID  = String
type Signal= String
type Wifi  = (SSID, Signal)
type Log   = String

data Command = Scan{ scan :: String} | Connect {connect :: String -> String}

instance Show Command where
  show (Scan _) = "Scanning for finding some Wifi"
  show (Connect _) = "Connecting to an elected Wifi..."


data CommandError = EmptyCommand| InvalidCommand| OtherError String  deriving (Show, Eq)

instance Error(CommandError) where
  noMsg = OtherError "Some problem occured during command execution"
  strMsg = OtherError

type ProcessMonad = ErrorT CommandError IO
