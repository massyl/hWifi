module Main where

import Test.HUnit
import Network.HWifi
import Network.Nmcli
import Network.Types
import Network.Utils ( run
                     , clean
                     , formatMsg
                     , split)

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

testRun0, testRun1, testRuns :: Test.HUnit.Test
testRun0 = TestCase $ run "" >>= assertEqual "Bad command - empty command" (Left $ BadCommand "")
testRun1 = TestCase $ run "ls -" >>= assertEqual "Bad command - incorrect command" (Left $ BadCommand "ls -")
testRun2 = TestCase $ run "echo 'tatooine':75\n'myrkr':90" >>= assertEqual "Retrieve output from the command" (Right ["'tatooine':75","'myrkr':90"])
testRuns = TestList [ "testRun0" ~: testRun0
                    , "testRun1" ~: testRun1
                    , "testRun2" ~: testRun2
                    ]

testFormatMsg0, testFormatMsgs :: Test.HUnit.Test
testFormatMsg0 = ["No known wifi available!"] ~=? formatMsg "" id (Left NoWifiAvailable)
testFormatMsg1 = [ "prefix string: "
                 , "- input 0"
                 , "- input 1"] ~=? formatMsg "prefix string: " ("- " ++) (Right ["input 0", "input 1"])
testFormatMsgs = TestList [ "testFormatMsg0" ~: testFormatMsg0
                          , "testFormatMsg1" ~: testFormatMsg1
                          ]

testSplit0, testSplit1, testSplit2, testSplits :: Test.HUnit.Test
testSplit0 = split "\r\n" "a\r\nb\r\nd\r\ne" ~=? ["a","b","d","e"]
testSplit1 = split "aaa"  "aaaXaaaXaaaXaaa"  ~=? ["","X","X","X",""]
testSplit2 = split "x"    "x"                ~=? ["",""]

testSplits = TestList [ "testSplit0" ~: testSplit0
                      , "testSplit1" ~: testSplit1
                      , "testSplit2" ~: testSplit2]
-- Full tests
tests :: Test.HUnit.Test
tests = TestList [ testCommandScanWifi
                 , testKnownCommand
                 , testCleanStrings
                 , testConnectToWifiCommands
                 , testElectWifis
                 , testRuns
                 , testFormatMsgs
                 , testSplits]

main :: IO ()
main = runTestTT tests >>= print
