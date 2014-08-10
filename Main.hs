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
-- Portability :  portable
-- Dependency  :  nmcli (network-manager package in debian-based platform - http://www.gnome.org/projects/NetworkManager/)
--
-- A module to deal with wifi connections.
-- At the moment, only election of the wifi with the most powerful signal and autoconnect policy.
--
-- Use: cabal run
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
                    , CommandError(..)
                    , ThrowsError)
import Network.HWifi ( runWifiMonad
                     , unsafeElect
                     , available
                     , alreadyUsed
                     , connectWifi)
import Control.Exception(evaluate)

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

-- | Log informational
output :: [Log]-> IO ()
output = mapM_ putStrLn

-- | Main orchestrator
-- Determine the highest known wifi signal and connect to it
main :: IO ()
main = do
  (allWifis, msg0)   <- availableWifisWithLogs scanCmd
  output msg0
  (knownWifis, msg1) <- alreadyUsedWifisWithLogs knownCmd
  output msg1
  (_ , msg2) <- join $ connectWifiWithLogs conCmd <$> elect knownWifis allWifis
  output msg2

-- | Returns available network wifis. It discards any logged message.
availableWifis :: Command -> IO (ThrowsError [SSID])
availableWifis scanCommand = fst <$> availableWifisWithLogs scanCommand

-- | Returns already used network wifis. It discards any logged message.
alreadyUsedWifis :: Command -> IO (ThrowsError [SSID])
alreadyUsedWifis knownCommand = fst <$> alreadyUsedWifisWithLogs knownCommand

-- -- | Returns elected wifi (wifi already known, available, with highest signal).
electedWifi :: Command -> Command -> IO (ThrowsError SSID)
electedWifi scanCommand knownCommand = join $ elect <$> alreadyUsedWifis knownCommand <*> availableWifis scanCommand
