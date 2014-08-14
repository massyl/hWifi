module Network.StandardPolicy ( elect
                              , availableWifisWithLogs
                              , alreadyUsedWifisWithLogs
                              , connectWifiWithLogs
                              , scanAndConnectToKnownWifiWithMostPowerfulSignal)
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
-- A standard policy connection module.
-- Determine the most powerful wifi signal amongst known auto-connect wifi and try and connect to it.
--
-----------------------------------------------------------------------------

import Control.Exception (evaluate)
import Control.Monad (join)
import Data.Functor ((<$>))
import Network.HWifi ( runWifiMonad
                     , unsafeElect
                     , available
                     , alreadyUsed
                     , connectWifi)
import Network.Types( SSID
                    , Log
                    , Command(..)
                    , ThrowsError)

-- | Elects wifi safely (runs in `IO` monad)
elect :: ThrowsError [SSID] -> ThrowsError [SSID] -> IO (ThrowsError SSID)
elect wifis = evaluate . unsafeElect wifis

-- | Returns the available network wifi list and records any logged message
availableWifisWithLogs :: Command -> IO (ThrowsError [SSID], [Log])
availableWifisWithLogs = runWifiMonad . available

-- | Returns already used network wifi list and record any logged message.
alreadyUsedWifisWithLogs :: Command -> IO (ThrowsError [SSID], [Log])
alreadyUsedWifisWithLogs = runWifiMonad . alreadyUsed

-- | Connect to wifi
connectWifiWithLogs :: Command -> ThrowsError SSID -> IO (ThrowsError [SSID], [Log])
connectWifiWithLogs cmd = runWifiMonad . connectWifi cmd

-- | Log output
output :: [Log]-> IO ()
output = mapM_ putStrLn

-- | Scan and connect to a known wifi with the most powerful signal
scanAndConnectToKnownWifiWithMostPowerfulSignal :: Command -> Command -> Command -> IO ()
scanAndConnectToKnownWifiWithMostPowerfulSignal scanCommand knownCommand conCommand = do
  (allWifis, msg0)   <- availableWifisWithLogs scanCommand
  output msg0
  (knownWifis, msg1) <- alreadyUsedWifisWithLogs knownCommand
  output msg1
  (result, msg2)     <- join $ connectWifiWithLogs conCommand <$> elect allWifis knownWifis
  case result of
    Left err -> putStrLn $ "\nError: " ++ show err
    Right _  -> output msg2
