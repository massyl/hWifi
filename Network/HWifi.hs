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
import Control.Monad.Writer hiding(mapM_)
import Data.Foldable hiding (mapM_)
import Prelude hiding(elem)
import Control.Monad.Error
import Control.Applicative((<*))

type Wifi w a = WriterT w IO a
data Command = Scan String | ListKnown String | Connect String

-- | Command to scan the current wifi
cScanWifi :: Command
cScanWifi = Scan "nmcli --terse --fields ssid,signal dev wifi"

-- | Command to list the wifi the computer can currently auto connect to
cKownWifi :: Command
cKownWifi = ListKnown "nmcli --terse --fields name con list"

-- | Given a wifi, execute the command to connect to a wifi (need super power :)
cConnectToWifi :: [String] -> String
cConnectToWifi [wifi] = "sudo nmcli con up id " ++ wifi

-- | Run a command and displays the output in list of strings
run :: String -> IO [String]
run command = readProcess comm args [] >>= return . lines
  where (comm:args) = words command

-- | Utility function to trim the ' in a string
cleanString :: String -> String
cleanString s = if (elem '\'' s) then tail . init $ s
                else s

-- | Slice a string "'wifi':signal" in a tuple ("wifi", "signal")
parseWifi :: String -> (String, String)
parseWifi s = (cleanString ssid, tail signal)
  where (ssid, signal) = break (== ':') s

-- | Given a list of signals, return the list of couple (wifi, signal)
parseWifis :: [String] -> [(String, String)]
parseWifis = map parseWifi

-- | Scan the proximity wifi and return a list of (ssid, signal).
scanWifis:: Command -> Wifi [String] [String]
scanWifis (Scan cmd) = runWithLog (map fst . reverse . sortBy (compare `on` snd) . map parseWifi <$> run cmd) logAll

-- execCommand :: (Foldable f) => Command -> Wifi [String] (f a)
-- execCommand (Scan cmd) = runWithLog (foldMap parseWifi <$> run cmd) logAll

-- | List the current wifi the computer can connect to
getKnownWifi :: Command -> Wifi [String] [String]
getKnownWifi (ListKnown cmd) = runWithLog (run cmd) logKnown

-- | Runs a computation and logs f on the computation results
runWithLog :: (Monoid b) => IO a -> (a -> b) -> Wifi b a
runWithLog comp f = do
  result <- liftIO comp
  tell $ f result
  return result

-- runWithLog :: (Monoid b) => IO a -> (a -> b) -> Wifi b a
-- runWithLog comp f = liftIO comp >>= (\result -> (tell $ f result) >>  return result)

-- | Filter the list of wifis the machine (in its current setup) can autoconnect to
retainKnown :: [String] -> [String] -> [String]
retainKnown known = filter $ (== True) . (`elem` known)

-- | Elect wifi according to signal's power (the most powerful is elected)
electWifi :: [String] -> [String]
electWifi (w : _) = [w]

-- | Elect wifi according to signal's power joined to a list of auto connect ones
electWifiFrom :: [String] -> [String] -> [String]
electWifiFrom known = electWifi . retainKnown known

-- | Log scanned wifi into list of formatted strings
logAll :: [String] -> [String]
logAll = ("Scanned wifi: \n" :) . map ("- "++)

-- | Log auto connect wifi into list of formatted strings
logKnown :: [String] -> [String]
logKnown = ("\n Auto-connect wifi: \n" :) . map ("- "++)

-- | Scan the wifi, compute the list of autoconnect wifis, connect to one (if multiple possible, the one with the most powerful signal is elected)
main :: IO ()
main = do
  (allWifis, msg1)   <- runWriterT $ scanWifis cScanWifi
  (knownWifis, msg2) <- runWriterT $ getKnownWifi cKownWifi
  run . cConnectToWifi $ electWifiFrom knownWifis allWifis
  -- run . commandConnectToWifi $ electWifiFrom knownWifis allWifis `catchError` return ["No wifi found"]
  mapM_ putStrLn $ msg1 ++ msg2
