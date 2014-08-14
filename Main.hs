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

import Control.Monad(join)
import Data.Functor((<$>))
import Control.Applicative((<*>))
import Network.Nmcli( conCmd
                    , scanCmd
                    , knownCmd)
import Network.Types( SSID
                    , Log
                    , Command(..)
                    , ThrowsError)
import Network.StandardPolicy ( scanAndConnectToKnownWifiWithMostPowerfulSignal
                              , alreadyUsedWifisWithLogs
                              , availableWifisWithLogs
                              , elect)

-- | Main orchestrator
-- Determine the highest known wifi signal and connect to it
main :: IO ()
main = scanAndConnectToKnownWifiWithMostPowerfulSignal scanCmd knownCmd conCmd

-- | Returns available network wifis. It discards any logged message.
availableWifis :: Command -> IO (ThrowsError [SSID])
availableWifis scanCommand = fst <$> availableWifisWithLogs scanCommand

-- | Returns already used network wifis. It discards any logged message.
alreadyUsedWifis :: Command -> IO (ThrowsError [SSID])
alreadyUsedWifis knownCommand = fst <$> alreadyUsedWifisWithLogs knownCommand

-- | Returns elected wifi (wifi already known, available, with highest signal).
electedWifi :: Command -> Command -> IO (ThrowsError SSID)
electedWifi scanCommand knownCommand = join $ elect <$> alreadyUsedWifis knownCommand <*> availableWifis scanCommand
