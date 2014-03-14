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

-- *Wifi> run "nmcli --terse --fields name con list"
-- ["myrkr","dantooine","tatooine"]
-- *Wifi> run "nmcli con list"
-- ["NAME                      UUID                                   TYPE              TIMESTAMP-REAL                    ","dantooine            68400207-92c9-4c8f-90b4-725b45c8359f   802-11-wireless   mar. 04 f\233vr. 2014 18:44:15 CET   ","myrkr                076684ca-6287-4625-bab6-524b865e185e   802-11-wireless   never                             ","tatooine                  deb87d57-aedc-46a8-8994-ce83c91ce522   802-11-wireless   sam. 08 f\233vr. 2014 13:04:56 CET   "]

-- Scan the wifi and return the ssid:signal
-- *Wifi> run "nmcli --terse --fields ssid,signal dev wifi"
-- ["'Livebox-0ff6':42","'tatooine':72"]

-- | Utility function to trim the ' in a string
cleanString :: String -> String
cleanString s = if (elem '\'' s) then tail . init $ s else s

-- | Slice a string "'wifi':signal" in a tuple ("wifi", "signal")
sliceSSIDSignal :: String -> (String, String)
sliceSSIDSignal s = (cleanString ssid, tail signal) where (ssid, signal) = break (== ':') s

-- | Given a list of signals, return the list of couple (wifi, signal)
sliceSSIDSignals :: [String] -> [(String, String)]
sliceSSIDSignals = map sliceSSIDSignal

-- | Scan the proximity wifi
scanWifi :: IO [(String, String)]
scanWifi =  map sliceSSIDSignal <$> run commandScanWifi

-- *Wifi> scanWifi
-- fromList [("Livebox-0ff6","42"),("freewifi","75")]

-- | List the current wifi the computer can connect to
listWifiAutoConnect :: IO [String]
listWifiAutoConnect = run commandListWifiAutoConnect

-- *Wifi> listWifiAutoConnect
-- ["dantooine","myrkr","tatooine"]

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
electWifiFrom scannedWifis autoConnectWifis =
  (electWifi . filterKnownWifi autoConnectWifis) scannedWifis

-- | Log message function
logMsg :: String -> [String] -> String -> String
logMsg _ [] _              = "No know wifi!"
logMsg prefix [msg] suffix = prefix ++ msg ++ suffix

-- | Log scanned wifi into list of formatted string
logScannedWifi :: [(String,String)] -> [String]
logScannedWifi = map (("- "++) . fst)

-- | Log auto connect wifi into list of formatted string
logAutoConnectWifi :: [String] -> [String]
logAutoConnectWifi = map ("- "++)

-- | Scan the wifi, compute the list of autoconnect wifis, connect to one (if multiple possible, the one with the most powerful signal is elected)
main :: IO ()
main = do
  scannedWifis     <- scanWifi
  autoConnectWifis <- listWifiAutoConnect
  let electedWifi = electWifiFrom scannedWifis autoConnectWifis
  (run . commandConnectToWifi) $ electedWifi
  mapM_ putStrLn $ ["Scanned wifi: "]
                 ++ logScannedWifi scannedWifis
                 ++ ["\nAuto-connect wifi: "]
                 ++ logAutoConnectWifi autoConnectWifis
                 ++ ["\nElect the most powerful wifi signal."
                    ,(logMsg "Connection to wifi '" electedWifi "'")]
