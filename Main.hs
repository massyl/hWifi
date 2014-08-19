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
-- Dependency  :  nmcli (network-manager package in debian-based platform - http://www.gnome.org/projects/NetworkManager/) + checkbox (System testing application)
--
-- A module to deal with wifi connections.
-- 2 use cases:
-- - Determine the most powerful wifi signal amongst known auto-connect wifis and connect to it.
-- - If provided with (ssid, wifiSecurity {wpa or wep}, psk) in this order, this will create a new auto-connect entry and connect to it.
--
-- Use:
-- - `cabal run` for standard auto-connect policy to known wifi
-- - `cabal run <ssid> <wifiSecurity> <psk>` to create a new auto-connect wifi entry
--
-----------------------------------------------------------------------------

import           Network.Nmcli          (conCmd, createCmd, knownCmd, scanCmd)
import           Network.StandardPolicy (alreadyUsedWifis, availableWifis,
                                         createNewWifiConnectionAndConnect,
                                         electedWifi, scanAndConnectToKnownWifiWithMostPowerfulSignal)
import           System.Console.GetOpt
import           System.Environment

data Options = Options { optVerbose       :: Bool
                       , optShowVersion   :: Bool
                       , optAuto          :: Bool
                       , optSSID          :: Maybe String
                       , optConnectPolicy :: Maybe String
                       , optPsk           :: Maybe String
                       } deriving Show

defaultOptions :: Options
defaultOptions    = Options { optVerbose       = False
                            , optShowVersion   = False
                            , optAuto          = True
                            , optSSID          = Nothing
                            , optConnectPolicy = Nothing
                            , optPsk           = Nothing
                            }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "v" ["verbose"]        (NoArg (\ opts -> opts { optVerbose = True }))                        "Chatty output on stderr"
  , Option "V?"["version"]        (NoArg (\ opts -> opts { optShowVersion = True }))                    "Show version number"
  , Option "a" ["auto"]           (NoArg (\ opts -> opts { optAuto = True }))                           "Standard auto-connect policy. This is the default behavior"
  , Option "s" ["ssid"]           (ReqArg (\ f opts -> opts { optSSID = Just f }) "SSID")               "wifi SSID to connect to."
  , Option "c" ["connect-policy"] (ReqArg (\ f opts -> opts { optConnectPolicy = Just f }) "<wpa|wep>") "The connection policy (wep or wpa)"
  , Option "p" ["psk"]            (ReqArg (\ f opts -> opts { optPsk = Just f }) "<psk>")               "Pre-Shared Key to connect to the ssid."
  ]

compilerOpts :: [String] -> IO Options
compilerOpts argv =
  case getOpt Permute options argv of
     (opts,_,[]) -> return $ foldl (flip id) defaultOptions opts
     (_,_,errs)  -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: hwifi [OPTION...] files..."

main :: IO ()
main =
  getArgs >>=
  compilerOpts >>=
  \ (Options { optVerbose = verboseFlag
             , optShowVersion = versionFlag
             , optAuto = autoFlag
             , optSSID = ssidOpt
             , optConnectPolicy = connectPolicyOpt
             , optPsk = pskOpt }) ->
               if autoFlag
                  then scanAndConnectToKnownWifiWithMostPowerfulSignal scanCmd knownCmd conCmd
                  else let (Just ssid, Just connectPolicy, Just psk) = (ssidOpt, connectPolicyOpt, pskOpt)
                       in createNewWifiConnectionAndConnect createCmd ssid connectPolicy psk
