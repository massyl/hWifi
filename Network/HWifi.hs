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

import Data.Functor((<$>))
import Data.List (intersect, sort)
import Control.Monad.Writer hiding(mapM_)
import Control.Arrow ((***), second)
import Network.Utils(catchIO, clean, run, logMsg)
import Control.Exception(evaluate)
import Network.Types(SSID, Log, Wifi, WifiMonad, Command(..), Output)

-- | Helper function, to run stack of monad transformers
runWifiMonad :: WifiMonad w a -> IO (a, w)
runWifiMonad  = runWriterT

-- | Runs a computation `comp`, get the result and logs the
-- | application of `logFn` on it and then return the computation.
runWithLog :: (Monoid b) => IO a -> (a -> b) -> WifiMonad b a
runWithLog comp logFn = do
  result <- liftIO comp
  tell $ logFn result
  return result

-- | Runs a given command, returns available wifis and reports any logged info.
available :: Command -> WifiMonad [Log][SSID]
available (Connect _) = tell ["Irrelevant Command Connect for 'available' function"] >> return []
available (Scan cmd)  = runWithLog wifis log
                        where readOutput = map (fst . second sort . parse)
                              -- | Slice a string "'wifi':signal" in a tuple ("wifi", "signal")
                              parse :: Output -> Wifi
                              parse = (clean '\'' *** tail) . break (== ':')
                              wifis = readOutput <$> run cmd
                              log = logMsg "Scanned wifi: \n" ("- "++)

-- | Returns already used wifis and reports any logged info.
alreadyUsed :: Command -> WifiMonad [Log][SSID]
alreadyUsed (Connect _) = tell ["Irrelevant Command Connect for 'alreadyUsed' function"] >> return []
alreadyUsed (Scan cmd)  = runWithLog wifis log
                          where readOutput = id
                                wifis = readOutput <$> run cmd
                                log = logMsg "\n Auto-connect wifi: \n" ("- "++)

-- | Elects wifi according to signal's power joined to a list of auto connect ones
-- | This function throws an exception if you give an empty `wifis` parameter
unsafeElect :: [SSID] -> [SSID] -> SSID
unsafeElect wifis = head . intersect wifis

-- | Elects wifi safely (runs in `IO` monad)
elect :: [SSID] -> [SSID] -> IO SSID
elect wifis = (`catchIO` []) . evaluate . unsafeElect wifis
