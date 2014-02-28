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
-- At the moment, only connection to a wifi with autoconnect policy.
--
-- Use: runhaskell Network/HWifi.hs
-----------------------------------------------------------------------------

import System.Process
import Data.Function (on)
import Data.Functor
import Data.List (sortBy)
import Control.Arrow

commandScanWifi :: Maybe String
commandScanWifi = Just "nmcli --terse --fields ssid,signal dev wifi"

commandScanWifiAutoConnect :: Maybe String
commandScanWifiAutoConnect = Just "nmcli --terse --fields name con list"

command :: String -> [String]
command = words

run :: Maybe String -> IO [String]
run Nothing            = return []
run (Just fullCommand) =
  do result <- readProcess comm args []
     return $ lines result
  where (comm:args) = command fullCommand

-- *Wifi> run "nmcli --terse --fields name con list"
-- ["myrkr","dantooine","tatooine"]
-- *Wifi> run "nmcli con list"
-- ["NAME                      UUID                                   TYPE              TIMESTAMP-REAL                    ","dantooine            68400207-92c9-4c8f-90b4-725b45c8359f   802-11-wireless   mar. 04 f\233vr. 2014 18:44:15 CET   ","myrkr                076684ca-6287-4625-bab6-524b865e185e   802-11-wireless   never                             ","tatooine                  deb87d57-aedc-46a8-8994-ce83c91ce522   802-11-wireless   sam. 08 f\233vr. 2014 13:04:56 CET   "]

-- Scan the wifi and return the ssid:signal
-- *Wifi> run "nmcli --terse --fields ssid,signal dev wifi"
-- ["'Livebox-0ff6':42","'tatooine':72"]

cleanString :: String -> String
cleanString s =
  if (elem '\'' s)
  then tail . init $ s
  else s

sliceSSIDSignal :: String -> (String, String)
sliceSSIDSignal s =
  (cleanString ssid, tail signal)
  where (ssid, signal) = break (== ':') s

sliceSSIDSignals :: [String] -> [(String, String)]
sliceSSIDSignals = map sliceSSIDSignal

scanWifi :: IO [(String, String)]
scanWifi =  map sliceSSIDSignal <$> run commandScanWifi

-- *Wifi> scanWifi
-- fromList [("Livebox-0ff6","42"),("freewifi","75")]

autoConnectWifi :: IO [String]
autoConnectWifi = run commandScanWifiAutoConnect

-- *Wifi> autoConnectWifi
-- ["dantooine","myrkr","tatooine"]

-- | Filter the list of wifis the machine (in its current setup) can autoconnect to
wifiToConnect :: [String] -> [(String,String)] -> [(String,String)]
--wifiToConnect autoConnectWifis scannedWifis = map (filter (map elem autoConnectWifis) . fst) scannedWifis
--wifiToConnect autoConnectWifis scannedWifis = filter (`elem` autoConnectWifis . fst) scannedWifis
wifiToConnect autoConnectWifis = filter $ (\(a, _)-> a == True) . first (`elem` autoConnectWifis)

connectToWifiCommand :: Maybe String -> Maybe String
connectToWifiCommand Nothing     = Nothing
connectToWifiCommand (Just wifi) = Just $ "nmcli con up id " ++ wifi

-- | elect wifi according to signal's power (the more powerful is elected)
electWifi :: [(String, String)] -> Maybe String
electWifi []      = Nothing
electWifi [(w,_)] = Just w
electWifi wifi    = Just . fst. head . sortBy (compare `on` snd) $ wifi

-- | Scan the wifi, compute the list of autoconnect wifis, connect to one (if multiple possible, the one with the most powerful signal is elected)
main :: IO ()
main = do
  putStrLn "\nElect the most powerful wifi signal."
  autoConnectWifis <- autoConnectWifi
  electWifi . wifiToConnect autoConnectWifis <$> scanWifi >>= run . connectToWifiCommand >> return ()
