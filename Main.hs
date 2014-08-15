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
--
-- Use: cabal run
--
-----------------------------------------------------------------------------

import Network.Nmcli( conCmd
                    , scanCmd
                    , knownCmd)
import Network.StandardPolicy ( availableWifis
                              , alreadyUsedWifis
                              , electedWifi
                              , scanAndConnectToKnownWifiWithMostPowerfulSignal)

-- | Main orchestrator
-- Determine the highest known wifi signal and connect to it
main :: IO ()
main = scanAndConnectToKnownWifiWithMostPowerfulSignal scanCmd knownCmd conCmd
