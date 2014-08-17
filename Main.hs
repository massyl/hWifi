module Main (main
            , availableWifis
            , alreadyUsedWifis
            , electedWifi)
       where

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Main
-- Copyright   :  (c) Commiters
-- License     :  The same as `nmcli` - http://manpages.ubuntu.com/manpages/maverick/man1/nmcli.1.html
--
-- Maintainer  :  massyl, ardumont
-- Stability   :  experimental
-- Portability :  unportable
-- Dependency  :  nmcli (network-manager package in debian-based platform - http://www.gnome.org/projects/NetworkManager/)
--
-- A module to deal with wifi connections.
-- Determine the most powerful wifi signal amongst known auto-connect wifi and try and connect to it.
-- If providing (ssid, wifiSecurity {wpa or wep}, psk), this will create a new auto-connect entry and connect to it.
--
-- Use:
-- - `cabal run` for standard auto-connect policy to known wifi
-- - `cabal run <ssid> <wifiSecurity> <psk>` to create a new auto-connect wifi entry
--
-----------------------------------------------------------------------------

import           Network.Nmcli          (conCmd, createCmd, knownCmd, scanCmd)
import           Network.StandardPolicy (alreadyUsedWifis, availableWifis,
                                         createNewWifiConnectionAndConnect,
                                         electedWifi, scanAndConnectToKnownWifiWithMostPowerfulSignal)
import           System.Environment

-- | Main orchestrator
-- Without argument: Determine the highest known wifi signal and connect to it
-- With 3 arguments (ssid, wifiSecurity, psk) in this order, create a new wifi session and connect to it
main :: IO ()
main = do
  args <- getArgs
  if null args
     then scanAndConnectToKnownWifiWithMostPowerfulSignal scanCmd knownCmd conCmd
     else let (ssid:wifiSecurity:psk:_) = args in
          createNewWifiConnectionAndConnect createCmd ssid wifiSecurity psk
