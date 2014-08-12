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
import Data.List (intersect, sort, sortBy)
import Control.Monad.Writer hiding(mapM_)
import Control.Arrow ((***), second)
import Network.Utils(clean, run, formatMsg, catchIO)
import Network.Types(SSID, Log, Signal, Wifi, WifiMonad, Command(..), Output, CommandError(..), ThrowsError)
import Data.Function (on)

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

-- -- | Runs a given command, returns available wifis and reports any logged info.
available :: Command -> WifiMonad [Log](ThrowsError [SSID])
available (Scan cmd)  = runWithLog wifis logMsg
                        where parseOutput :: ThrowsError [SSID] -> ThrowsError [SSID]
                              parseOutput input = case input of
                                Left err    -> Left err
                                Right ssids -> Right $ (map fst . reverse . sortBy (compare `on` snd) . map parse) ssids
                                           where -- | Slice a string "'wifi':signal" in a tuple ("wifi", signal)
                                                 parse :: Output -> Wifi
                                                 parse = (\ (x, y) -> (x, read y :: Signal)) . (clean '\'' *** tail) . break (== ':')
                              wifis :: IO (ThrowsError [SSID])
                              wifis = parseOutput <$> run cmd `catchIO` Left ScanWifiError
                              logMsg :: ThrowsError [SSID] -> [Log]
                              logMsg = formatMsg "Scanned wifi: \n" ("- "++)

-- -- | Returns already used wifis and reports any logged info.
alreadyUsed :: Command -> WifiMonad [Log](ThrowsError [SSID])
alreadyUsed (Scan cmd)  = runWithLog wifis logMsg
                          where parseOutput = id
                                wifis = parseOutput <$> run cmd `catchIO` Left KnownWifiError
                                logMsg = formatMsg "\nAuto-connect wifi: \n" ("- "++)

-- -- | Connect to wifi
connectWifi :: Command -> ThrowsError SSID -> WifiMonad [Log](ThrowsError [SSID])
connectWifi _ (Left err)                     = return $ Left err
connectWifi (Connect connectFn) (Right ssid) =
  runWithLog wifis logMsg
  where parseOutput = id
        wifis = parseOutput <$> run (connectFn ssid) `catchIO` (Left $ ConnectionError ssid)
        logMsg = formatMsg ("\nConnection to wifi '" ++ ssid ++ "'") id

-- | Elects wifi according to signal's power joined to a list of auto connect ones
-- | This function throws an exception if you give an empty `wifis` parameter
unsafeElect :: ThrowsError [SSID] -> ThrowsError [SSID] -> ThrowsError SSID
unsafeElect (Left m)     _                   = Left m
unsafeElect _            (Left m)            = Left m
unsafeElect (Right [])   _                   = Left NoWifiAvailable
unsafeElect (Right wifis) (Right knownsWifi) = case wifis `intersect` knownsWifi of
  []    -> Left NoWifiAvailable
  (x:_) -> Right x
