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
import Data.List (sortBy, intersect, delete, isPrefixOf)
import Control.Monad.Writer hiding(mapM_)
import Data.Foldable hiding (mapM_)
import Prelude hiding(elem)
import Control.Monad.Error
import Control.Arrow (first, (***))
type Wifi w a = WriterT w IO a
data Command = Scan String | ListKnown String | Connect String

{--
  Command to scan the current wifi
--}
scanCmd :: Command
scanCmd = Scan "nmcli --terse --fields ssid,signal dev wifi"

-- | Command to list the wifi the computer can currently auto connect to
knownCmd :: Command
knownCmd = ListKnown "nmcli --terse --fields name con list"

-- | Given a wifi, execute the command to connect to a wifi (need super power :)
connectCmd :: String -> String
connectCmd = ("sudo nmcli con up id " ++)

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

-- | Elect wifi according to signal's power joined to a list of auto connect ones
elect :: [String] -> [String] -> String
elect known = head . intersect known

logMsg :: String -> (String -> String) -> [String] -> [String]
logMsg prefix f = (prefix :) . map f

{-- Scan the wifi, compute the list of autoconnect wifis, connect to one (if multiple possible,
    the one with the most powerful signal is elected)
--}
main :: IO ()
main = do
  (allWifis, msg1)   <- runWriterT $ scanWifis scanCmd
  (knownWifis, msg2) <- runWriterT $ getKnownWifi knownCmd
  run . connectCmd $ elect knownWifis allWifis
  -- run . commandConnectToWifi $ elect knownWifis allWifis `catchError` return ["No wifi found"]
  mapM_ putStrLn $ msg1 ++ msg2
