module Network.HWifi where

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HWifi
-- Copyright   :  (c) Commiters
-- License     :  The same as `nmcli` - http://manpages.ubuntu.com/manpages/maverick/man1/nmcli.1.html
--
-- Maintainer  :  massyl, ardumont
-- Stability   :  experimental
-- Portability :  portable
-- Dependency  :  nmcli (network-manager package in debian-based platform - http://www.gnome.org/projects/NetworkManager/)
--
-- A simple module to deal with wifi connections.
-- At the moment, only election of the wifi with the most powerful signal and autoconnect policy.
--
-- Use: runhaskell Network/HWifi.hs
-----------------------------------------------------------------------------

import System.Process
import Data.Functor
import Data.List (intersect, delete, isPrefixOf, sort)
import Control.Monad.Writer hiding(mapM_)
import Prelude hiding(elem)
import Control.Monad.Error
import Control.Arrow ((***), second)

type WifiMonad w a = WriterT w IO a

type Wifi = (String, String)

type SSID = String

data Command = Scan{ scan :: String} | Connect {connect :: String -> String}

{--
  Command to scan the current wifi
--}
scanCmd :: Command
scanCmd = Scan "nmcli --terse --fields ssid,signal dev wifi"

-- | Command to list the wifi the computer can currently auto connect to
knownCmd :: Command
knownCmd = Scan "nmcli --terse --fields name con list"

-- | Given a wifi, execute the command to connect to a wifi (need super power :)
conCmd :: Command
conCmd = Connect ("sudo nmcli con up id " ++)

-- | Run a command and displays the output in list of strings
run :: String -> IO [String]
run command = readProcess comm args [] >>= return . lines
  where (comm:args) = words command

-- | Utility function to trim the ' in a string
clean :: Char -> String -> String
clean c cs = if isPrefixOf [c] cs then sanitize cs else cs
  where sanitize = delChar . reverse . delChar . reverse
        delChar  = delete c

-- | Slice a string "'wifi':signal" in a tuple ("wifi", "signal")
parse :: String -> Wifi
parse s = wifiDetail
  where wifiDetail = clean ('\'') *** tail $ break (== ':') s

available:: Command -> WifiMonad [String] [SSID]
available (Scan cmd) = runWithLog allWifis logAll
  where allWifis = (map (fst . second sort) . map parse) <$> run cmd
        logAll = logMsg ("Scanned wifi: \n") ("- "++)


-- | List the current wifi the computer can connect to
alreadyUsed :: Command -> WifiMonad [String] [SSID]
alreadyUsed (Scan cmd) = runWithLog (run cmd) logKnown
  where logKnown = logMsg ("\n Auto-connect wifi: \n") ("- "++)

-- | Runs a computation and logs f on the computation results
runWithLog :: (Monoid b) => IO a -> (a -> b) -> WifiMonad b a
runWithLog comp f = do
  result <- liftIO comp
  tell $ f result
  return result

-- | Elect wifi according to signal's power joined to a list of auto connect ones
elect :: [String] -> [SSID] -> SSID
elect known = head . intersect known

logMsg :: String -> (String -> String) -> [String] -> [String]
logMsg prefix f = (prefix :) . map f

{-- Scan the wifi, compute the list of autoconnect wifis, connect to one (if multiple possible,
    the one with the most powerful signal is elected)
--}
main :: IO ()
main = do
  (allWifis, msg1)   <- runWriterT $ available scanCmd
  (knownWifis, msg2) <- runWriterT $ alreadyUsed knownCmd
  run . connect conCmd $ elect knownWifis allWifis
  -- run . commandConnectToWifi $ elect knownWifis allWifis `catchError` return ["No wifi found"]
  mapM_ putStrLn $ msg1 ++ msg2
