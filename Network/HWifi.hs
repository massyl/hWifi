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

-- | Command to scan the current wifi
commandScanWifi :: Maybe String
commandScanWifi = Just "nmcli --terse --fields ssid,signal dev wifi"

-- | Command to list the wifi the computer can currently auto connect to
commandListWifiAutoConnect :: Maybe String
commandListWifiAutoConnect = Just "nmcli --terse --fields name con list"

-- | Function to split command into list of strings
command :: String -> [String]
command = words

-- | Run a command and displays the output in list of strings
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

-- | Given a wifi, execute the command to connect to a wifi
commandConnectToWifi :: Maybe String -> Maybe String
commandConnectToWifi Nothing     = Nothing
commandConnectToWifi (Just wifi) = Just $ "nmcli con up id " ++ wifi

-- | Elect wifi according to signal's power (the most powerful is elected)
electWifi :: [(String, String)] -> Maybe String
electWifi []      = Nothing
electWifi [(w,_)] = Just w
electWifi wifi    = Just . fst. head . sortBy (compare `on` snd) $ wifi

connectToWifiMsg :: Maybe String -> String
connectToWifiMsg Nothing     = "No connection possible!"
connectToWifiMsg (Just wifi) = "Connection to wifi '" ++ wifi ++ "'..."

connectedWifiMsg :: Maybe String -> String
connectedWifiMsg Nothing     = "No known wifi!"
connectedWifiMsg (Just wifi) = "Connection to wifi '" ++ wifi ++ "' successfully established!"

-- | Scan the wifi, compute the list of autoconnect wifis, connect to one (if multiple possible, the one with the most powerful signal is elected)
main :: IO ()
main = do
  scannedWifis <- scanWifi
  putStrLn "Scanned wifi: "
  mapM_ putStrLn $ map (("- "++) . fst) scannedWifis
  autoConnectWifis <- listWifiAutoConnect
  putStrLn "\nAuto-connect wifi: "
  mapM_ putStrLn $ map ("- "++) autoConnectWifis
  putStrLn "\nElect the most powerful wifi signal."
  electedWifi <- return $ (electWifi . filterKnownWifi autoConnectWifis) scannedWifis
  putStrLn (connectToWifiMsg electedWifi)
  (run . commandConnectToWifi) electedWifi
  putStrLn (connectedWifiMsg electedWifi)
