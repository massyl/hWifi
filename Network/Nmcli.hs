module Network.Nmcli ( scanCmd
                     , knownCmd
                     , conCmd
                     , createCmd)
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
-- Module exposing primitive commands using nmcli.
--
-----------------------------------------------------------------------------

import           Network.Types (Command (..))

-- |  Command to scan the current wifi
scanCmd :: Command
scanCmd = Scan "nmcli --terse --fields ssid,signal dev wifi"

-- | Command to list the wifi the computer can currently auto connect to
knownCmd :: Command
knownCmd = Scan "nmcli --terse --fields name con list"

-- | Given a wifi, execute the command to connect to a wifi (need super power :)
conCmd :: Command
conCmd = Connect ("sudo nmcli con up id " ++)

-- | Given a wifi, execute the creation of a new wifi entry
createCmd :: Command
createCmd = Create ( \ ssid wifiSecurity psk -> "sudo /usr/share/checkbox/scripts/create_connection wifi -S " ++ wifiSecurity ++ " -K " ++ psk ++ " " ++ ssid )
