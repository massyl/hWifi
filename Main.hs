module Main
       (main,
        availableWifis,
        alreadyUsedWifis,
        electedWifi) where

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

import Control.Monad (join)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Network.HWifi (runWifiMonad,
                      safeConnect,
                      safeElect,
                      knownCmd,
                      scanCmd,
                      alreadyUsed,
                      available,
                      SSID)

-- | Scan the wifi, compute the list of autoconnect wifis, connect to one (if multiple possible,
--    the one with the most powerful signal is elected)
availableWifisWithLogs :: IO ([SSID], [String])
availableWifisWithLogs =  runWifiMonad $ available scanCmd

availableWifis :: IO([SSID])
availableWifis = fst <$> availableWifisWithLogs

alreadyUsedWifisWithLogs :: IO ([SSID], [String])
alreadyUsedWifisWithLogs = runWifiMonad $ alreadyUsed knownCmd

alreadyUsedWifis :: IO([SSID])
alreadyUsedWifis = fst <$> alreadyUsedWifisWithLogs

electedWifi :: IO SSID
electedWifi = join $ safeElect <$> alreadyUsedWifis <*> availableWifis

logAll:: [String]-> IO ()
logAll = mapM_ putStrLn

main :: IO ()
main = do
  (allWifis, msg1)   <- availableWifisWithLogs
  logAll msg1
  (knownWifis, msg2) <- alreadyUsedWifisWithLogs
  logAll msg2
  let elected = (safeElect knownWifis allWifis)
  _ <- join $ safeConnect <$> elected
  elected >>= putStrLn . ("\n Elected Wifi: "++)
