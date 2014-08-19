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

data Flag = Verbose
          | Version
          | Auto
          | PSK String
          | SSID String
          | ConnectPolicy String
          | LibDir String
          deriving Show

options :: [OptDescr Flag]
options = [ Option "v"  ["verbose"]        (NoArg Verbose)                         "Chatty output on stderr"
          , Option "V?" ["version"]        (NoArg Version)                         "Show version number"
          , Option "a"  ["auto"]           (NoArg Auto)                            "Auto-connect policy"
          , Option "s"  ["ssid"]           (ReqArg SSID "SSID")                    "SSID to connect to"
          , Option "c"  ["connect-policy"] (ReqArg ConnectPolicy "connect-policy") "wifi connect policy: wpa or wep"
          , Option "p"  ["psk"]            (ReqArg PSK "pre-shared-key")           "Pre-shared key to connect to the wifi"
          ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
   case getOpt Permute options argv of
      (opts, nonOpts,[]) -> return (opts, nonOpts)
      (_,_,errs)         -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."

-- | Main orchestrator
-- Without argument: Determine the highest known wifi signal and connect to it
-- With 3 arguments (ssid, wifiSecurity, psk) in this order, create a new wifi session and connect to it
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let (argv, _) = compilerOpts args in
--   case any (== Auto) argv of
--     True -> scanAndConnectToKnownWifiWithMostPowerfulSignal scanCmd knownCmd conCmd
--     _     -> let (ssid:wifiSecurity:psk:_) = args in
--              createNewWifiConnectionAndConnect createCmd ssid wifiSecurity psk

main :: IO ()
main =
  getArgs >>=  compilerOpts >>= \flags -> mapM_ print $ fst flags
