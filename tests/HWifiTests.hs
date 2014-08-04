module Main where

import Network.HWifi
import Network.Utils
import Test.HUnit
import Network.Nmcli
import Network.Types

quote::Char
quote = '\''

testCommandScanWifi, testKnownCommand :: Test.HUnit.Test
testCommandScanWifi = "nmcli --terse --fields ssid,signal dev wifi" ~=? scan scanCmd
testKnownCommand = "nmcli --terse --fields name con list"           ~=? scan knownCmd

testCleanString1, testCleanString2, testCleanString3, testCleanString4, testCleanStrings :: Test.HUnit.Test
testCleanString1 = "hello" ~=? clean quote "'hello'"
testCleanString2 = "hello" ~=? clean quote "'hello"
testCleanString3 = "hello" ~=? clean quote "hello"
testCleanString4 = "hello'" ~=? clean quote "hello'"
testCleanStrings = TestList ["testCleanString1" ~: testCleanString1
                             ,"testCleanString2" ~: testCleanString2
                             ,"testCleanString3" ~: testCleanString3
                             ,"testCleanString4" ~: testCleanString4]

testSliceSSIDSignal1, testSliceSSIDSignal2, testSliceSSIDSignals :: Test.HUnit.Test
testSliceSSIDSignal1 = ("ssid","signal") ~=? parse "ssid:signal"
testSliceSSIDSignal2 = ("ssid", "signal") ~=? parse "'ssid':signal"
testSliceSSIDSignals = TestList ["testSliceSSIDSignal1" ~: testSliceSSIDSignal1
                                ,"testSliceSSIDSignal2" ~: testSliceSSIDSignal2]


-- testWifiToConnect1 :: Test.HUnit.Test
-- testWifiToConnect1 = [("tatooine", "67")]
--                      ~=?
--                      filterKnownWifi ["AndroidAP-tony","myrkr","tatooine"] [("Livebox-0ff6","42"),("tatooine","67")]

-- testWifiToConnect2 :: Test.HUnit.Test
-- testWifiToConnect2 = [("tatooine", "67"), ("dantooine", "72")]
--                      ~=?
--                      filterKnownWifi ["myrkr","dantooine","tatooine"] [("Livebox-0ff6","42"),("tatooine","67"),("dantooine", "72")]

-- testWifiToConnects :: Test.HUnit.Test
-- testWifiToConnects = TestList ["testWifiToConnect1" ~: testWifiToConnect1
--                                ,"testWifiToConnect2" ~: testWifiToConnect2]

testConnectToWifiCommand1, testConnectToWifiCommand2, testConnectToWifiCommands :: Test.HUnit.Test
testConnectToWifiCommand1 = "sudo nmcli con up id tatooine" ~=? connect conCmd "tatooine"
testConnectToWifiCommand2 = "sudo nmcli con up id "         ~=? connect conCmd []
testConnectToWifiCommands = TestList ["testConnectToWifiCommand1" ~: testConnectToWifiCommand1
                                     ,"testConnectToWifiCommand2" ~: testConnectToWifiCommand2]


testElectWifi1, testElectWifis :: Test.HUnit.Test
testElectWifi1 = "some-wifi-alone" ~=? unsafeElect ["some-wifi-alone", "wifi2", "wifi3"] ["some-wifi-alone","known1", "known2"]
testElectWifis = TestList ["testElectWifi1" ~: testElectWifi1]

-- Full tests
tests :: Test.HUnit.Test
tests = TestList [testCommandScanWifi
                  ,testKnownCommand
                  ,testCleanStrings
                  ,testSliceSSIDSignals
                  --,testSliceSSIDSignalss
                 -- ,testWifiToConnects
                  ,testConnectToWifiCommands
                  ,testElectWifis]

main :: IO ()
main = runTestTT tests >>= print
