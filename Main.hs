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
-- A module to deal with wifi connections, 2 main use cases:
-- - Automatic: Determine the most powerful wifi signal amongst known auto-connect wifis and connect to it.
-- - Manual: If provided with (ssid, connection policy {wpa or wep}, and pre-shared key), this will create a new auto-connect entry and connect to it.
--
-- Usage: hwifi [OPTION...] files...
--  -V, -?        --version                   Show version number
--  -a            --auto                      Standard auto-connect policy. This is the default behavior
--  -s SSID       --ssid=SSID                 wifi SSID to connect to.
--  -c <wpa|wep>  --connect-policy=<wpa|wep>  The connection policy (wep or wpa)
--  -p <psk>      --psk=<psk>                 Pre-Shared Key to connect to the ssid.
--
-- For example:
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

data Options = Options { optShowVersion   :: Bool
                       , optAuto          :: Bool
                       , optSSID          :: Maybe String
                       , optConnectPolicy :: Maybe String
                       , optPsk           :: Maybe String
                       } deriving Show

defaultOptions :: Options
defaultOptions    = Options { optShowVersion   = False
                            , optAuto          = True
                            , optSSID          = Nothing
                            , optConnectPolicy = Nothing
                            , optPsk           = Nothing
                            }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "V?"["version"]        (NoArg (\ opts -> opts { optShowVersion = True }))                    "Show version number"
  , Option "a" ["auto"]           (NoArg (\ opts -> opts { optAuto = True }))                           "Standard auto-connect policy. This is the default behavior"
  , Option "s" ["ssid"]           (ReqArg (\ f opts -> opts { optSSID = Just f }) "SSID")               "wifi SSID to connect to."
  , Option "c" ["connect-policy"] (ReqArg (\ f opts -> opts { optConnectPolicy = Just f }) "<wpa|wep>") "The connection policy (wep or wpa)"
  , Option "p" ["psk"]            (ReqArg (\ f opts -> opts { optPsk = Just f }) "<psk>")               "Pre-Shared Key to connect to the ssid."
  ]

usage :: [String] -> IO a
usage errs = ioError (userError (concat errs ++ usageInfo header options))
             where header = "\nUsage: hwifi [OPTION...] files..."

compilerOpts :: [String] -> IO Options
compilerOpts argv =
  case getOpt Permute options argv of
     (opts,_,[]) -> return $ foldl (flip id) defaultOptions opts
     (_,_,errs)  -> usage errs

version :: String
version = "0.0.0.1"

eval :: Options -> IO ()
eval (Options { optShowVersion = True }) = putStrLn $ "hWifi" ++ version
eval (Options { optAuto = True
              , optSSID = Nothing
              , optConnectPolicy = Nothing
              , optPsk = Nothing })      = scanAndConnectToKnownWifiWithMostPowerfulSignal scanCmd knownCmd conCmd
eval (Options { optSSID = (Just ssid)
              , optConnectPolicy = (Just connectPolicy)
              , optPsk = (Just psk) })   = createNewWifiConnectionAndConnect createCmd ssid connectPolicy psk
eval _                                   = usage ["Unknown options - Either use automatic connection (-a) or provide ssid (-s) with connect-policy (-c) and psk (-p)."]

main :: IO ()
main = getArgs >>= compilerOpts >>= eval
