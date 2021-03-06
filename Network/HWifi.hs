module Network.HWifi ( runWifiMonad
                     , runWithLog
                     , available
                     , alreadyUsed
                     , connectToWifi
                     , unsafeElect
                     , createNewWifi)
       where

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HWifi
-- Copyright   :  (c) Commiters
-- License     :  The same as `nmcli` - http://manpages.ubuntu.com/manpages/maverick/man1/nmcli.1.html
--
-- Maintainer  :  massyl, ardumont
-- Stability   :  experimental
-- Portability :  unportable
-- Dependency  :  nmcli (network-manager package in debian-based platform - http://www.gnome.org/projects/NetworkManager/)
--
-- Module exposing primitive functions to orchestrate wifi connections.
--
-----------------------------------------------------------------------------

import           Control.Arrow        (second, (***))
import           Control.Monad.Writer hiding (mapM_)
import           Data.Function        (on)
import           Data.Functor         ((<$>))
import           Data.List            (intersect, sortBy)
import           Network.Types        (Command (..), CommandError (..), Log,
                                       Output, Psk, SSID, ThrowsError, Wifi,
                                       WifiMonad)
import           Network.Utils        (catchIO, clean, formatMsg, run)

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
available :: Command -> WifiMonad [Log](ThrowsError [SSID])
available (Scan cmd)  = runWithLog wifis logMsg
                        where parseOutput :: ThrowsError [SSID] -> ThrowsError [SSID]
                              parseOutput = fmap (map fst . sortBy (flip compare `on` snd) . map parse)
                                                  where parse :: Output -> Wifi
                                                        parse = second (\y -> read y :: Integer) . (clean '\'' *** tail) . break (== ':')
                              wifis :: IO (ThrowsError [SSID])
                              wifis = parseOutput <$> run cmd `catchIO` Left ScanWifiError
                              logMsg :: ThrowsError [SSID] -> [Log]
                              logMsg = formatMsg "Scanned wifi: \n" ("- "++)
available c = (return . Left . BadCommand . show) c

-- | Returns already used wifis and reports any logged info.
alreadyUsed :: Command -> WifiMonad [Log](ThrowsError [SSID])
alreadyUsed (Scan cmd)  = runWithLog wifis logMsg
                          where parseOutput = id
                                wifis = parseOutput <$> run cmd `catchIO` Left KnownWifiError
                                logMsg = formatMsg "\nAuto-connect wifi: \n" ("- "++)
alreadyUsed c = (return . Left . BadCommand . show) c

-- | Connect to wifi
connectToWifi :: Command -> ThrowsError SSID -> WifiMonad [Log](ThrowsError [SSID])
connectToWifi _ (Left err)                     = return $ Left err
connectToWifi (Connect connectFn) (Right ssid) =
  runWithLog wifis logMsg
  where parseOutput = id
        wifis = parseOutput <$> run (connectFn ssid) `catchIO` (Left $ ConnectionError ssid)
        logMsg = formatMsg ("\nConnection to wifi '" ++ ssid ++ "'") id
connectToWifi c _ = (return . Left . BadCommand . show) c

-- | Create a new wifi and connect to it
createNewWifi :: Command -> SSID -> Psk -> WifiMonad [Log](ThrowsError [SSID])
createNewWifi _ [] _ = (return . Left . EmptySSID) "SSID must be specified!"
createNewWifi (Create createFn) ssid psk =
  runWithLog wifis logMsg
  where parseOutput = id
        wifis = parseOutput <$> run (createFn ssid psk) `catchIO` (Left $ ConnectionCreationError ssid)
        logMsg = formatMsg ("\nCreation of the wifi connection '" ++ ssid ++ "' and connection") id
createNewWifi c _ _ = (return . Left . BadCommand . show) c

-- | Elects wifi according to signal's power joined to a list of auto connect ones
-- | This function throws an exception if you give an empty `wifis` parameter
unsafeElect :: ThrowsError [SSID] -> ThrowsError [SSID] -> ThrowsError SSID
unsafeElect (Left m)     _                   = Left m
unsafeElect _            (Left m)            = Left m
unsafeElect (Right [])   _                   = Left NoWifiAvailable
unsafeElect (Right wifis) (Right knownsWifi) = case wifis `intersect` knownsWifi of
  []    -> Left NoWifiAvailable
  (x:_) -> Right x
