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
import Network.Utils(run)
import Network.Nmcli( conCmd
                    , scanCmd
                    , knownCmd)
import Network.Types( SSID
                    , Log
                    , Command(..))
import Network.HWifi ( runWifiMonad
                     , elect
                     , availableWifisWithLogs
                     , alreadyUsedWifisWithLogs
                     , connectWifiWithLogs)

-- | Log informational
output :: [Log]-> IO ()
output = mapM_ putStrLn

-- | Main orchestrator
-- Determine the highest known wifi signal and connect to it
main :: IO ()
main = do
  (allWifis, log)   <- availableWifisWithLogs scanCmd
  output log
  (knownWifis, log) <- alreadyUsedWifisWithLogs knownCmd
  output log
  let elected = elect knownWifis allWifis
  (_ , log) <- join $ connectWifiWithLogs conCmd <$> elected
  output log

-- | Returns available network wifis. It discards any logged message.
availableWifis :: Command -> IO [SSID]
availableWifis scanCmd = fst <$> availableWifisWithLogs scanCmd

-- | Returns already used network wifis. It discards any logged message.
alreadyUsedWifis :: Command -> IO [SSID]
alreadyUsedWifis knownCmd = fst <$> alreadyUsedWifisWithLogs knownCmd

-- | Returns elected wifi (wifi already known, available, with highest signal).
electedWifi :: Command -> Command -> IO SSID
electedWifi scanCmd knownCmd = join $ elect <$> alreadyUsedWifis knownCmd <*> availableWifis scanCmd
