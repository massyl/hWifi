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
import Data.Function (on)
import Data.Functor
import Data.List (sortBy)
import Control.Arrow
import Control.Monad.Writer


type Wifi w a = WriterT w IO a

-- | Command to scan the current wifi
commandScanWifi :: String
commandScanWifi = "nmcli --terse --fields ssid,signal dev wifi"

-- | Command to list the wifi the computer can currently auto connect to
commandListWifiAutoConnect :: String
commandListWifiAutoConnect = "nmcli --terse --fields name con list"

-- | Given a wifi, execute the command to connect to a wifi (need super power :)
commandConnectToWifi :: [String] -> String
commandConnectToWifi []     = []
commandConnectToWifi [wifi] = "sudo nmcli con up id " ++ wifi

-- | Run a command and displays the output in list of strings
run :: String -> IO [String]
run command = readProcess comm args [] >>= return . lines
  where (comm:args) = words command

-- | Utility function to trim the ' in a string
cleanString :: String -> String
cleanString s = if (elem '\'' s) then tail . init $ s else s

-- | Slice a string "'wifi':signal" in a tuple ("wifi", "signal")
sliceSSIDSignal :: String -> (String, String)
sliceSSIDSignal s = (cleanString ssid, tail signal) where (ssid, signal) = break (== ':') s

-- | Given a list of signals, return the list of couple (wifi, signal)
sliceSSIDSignals :: [String] -> [(String, String)]
sliceSSIDSignals = map sliceSSIDSignal

-- | Scan the proximity wifi and return a list of (ssid, signal).
scanWifi':: String -> Wifi [String] [(String,String)]
scanWifi' cmd = runWithLog (map sliceSSIDSignal <$> run cmd) logScannedWifi

-- | List the current wifi the computer can connect to
listWifiAutoConnect' :: String -> Wifi [String] [String]
listWifiAutoConnect' cmd = runWithLog (run cmd) logAutoConnectWifi

-- | Runs a computation and logs f on the computation results
runWithLog :: (Monoid b) => IO a -> (a -> b) -> Wifi b a
runWithLog comp f = do
  result <- liftIO comp
  tell $ f result
  return result

-- | Filter the list of wifis the machine (in its current setup) can autoconnect to
filterKnownWifi :: [String] -> [(String,String)] -> [(String,String)]
filterKnownWifi autoConnectWifis = filter $ (== True) . fst . first (`elem` autoConnectWifis)

-- | Elect wifi according to signal's power (the most powerful is elected)
electWifi :: [(String, String)] -> [String]
electWifi []      = []
electWifi [(w,_)] = [w]
electWifi wifi    = flip (:) [] . fst . head . sortBy (compare `on` snd) $ wifi

-- | Elect wifi according to signal's power joined to a list of auto connect ones
electWifiFrom :: [(String, String)] -> [String] -> [String]
electWifiFrom scannedWifis autoConnectWifis = (electWifi . filterKnownWifi autoConnectWifis) scannedWifis

-- | Log scanned wifi into list of formatted strings
logScannedWifi :: [(String,String)] -> [String]
logScannedWifi = ("Scanned wifi: \n" :) . map (("- "++) . fst)

-- | Log auto connect wifi into list of formatted strings
logAutoConnectWifi :: [String] -> [String]
logAutoConnectWifi = ("\n Auto-connect wifi: \n" :) . map ("- "++)

-- | Scan the wifi, compute the list of autoconnect wifis, connect to one (if multiple possible, the one with the most powerful signal is elected)
main :: IO ()
main = do
  (scannedWifis, msg1)  <- runWriterT $ scanWifi' commandScanWifi
  (autoConnectWifis, msg2) <- runWriterT $ listWifiAutoConnect' commandListWifiAutoConnect
  let electedWifi = electWifiFrom scannedWifis autoConnectWifis
  (run . commandConnectToWifi) electedWifi
  mapM_ putStrLn $ msg1 ++ msg2
