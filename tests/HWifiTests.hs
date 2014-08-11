module Main where

import Test.HUnit
import Network.HWifi
import Network.Utils
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

testConnectToWifiCommand1, testConnectToWifiCommand2, testConnectToWifiCommands :: Test.HUnit.Test
testConnectToWifiCommand1 = "sudo nmcli con up id tatooine" ~=? connect conCmd "tatooine"
testConnectToWifiCommand2 = "sudo nmcli con up id "         ~=? connect conCmd []
testConnectToWifiCommands = TestList ["testConnectToWifiCommand1" ~: testConnectToWifiCommand1
                                     ,"testConnectToWifiCommand2" ~: testConnectToWifiCommand2]


testElectWifi1, testElectWifis :: Test.HUnit.Test
testElectWifi1 = Right "some-wifi-alone" ~=? unsafeElect (Right ["some-wifi-alone", "wifi2", "wifi3"])
                                                         (Right ["some-wifi-alone","known1", "known2"])
testElectWifi2 = Left NoWifiAvailable ~=? unsafeElect (Right []) (Right ["some-wifi-alone","known1", "known2"])
testElectWifi3 = Left NoWifiAvailable ~=? unsafeElect (Right ["wifi0"]) (Right ["some-wifi-alone"])
testElectWifi4 = Left NoWifiAvailable ~=? unsafeElect (Left NoWifiAvailable) (Right ["some-wifi-alone"])
testElectWifi5 = Left KnownWifiError ~=? unsafeElect (Right ["wifi0"]) (Left KnownWifiError)
testElectWifis = TestList ["testElectWifi1" ~: testElectWifi1
                          ,"testElectWifi2" ~: testElectWifi2
                          ,"testElectWifi3" ~: testElectWifi3
                          ,"testElectWifi4" ~: testElectWifi4
                          ,"testElectWifi5" ~: testElectWifi5
                          ]

-- Full tests
tests :: Test.HUnit.Test
tests = TestList [testCommandScanWifi
                  ,testKnownCommand
                  ,testCleanStrings
                  -- ,testSliceSSIDSignals
                  --,testSliceSSIDSignalss
                 -- ,testWifiToConnects
                  ,testConnectToWifiCommands
                  ,testElectWifis]

main :: IO ()
main = runTestTT tests >>= print
