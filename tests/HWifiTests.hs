module Main where

import Test.HUnit
import Network.HWifi
import Network.Nmcli
import Network.Types
import Network.Utils ( run
                     , clean
                     , formatMsg
                     , split)


testCommandScanWifi, testKnownCommand :: Test.HUnit.Test
testCommandScanWifi = "Nmcli - Scan command"            ~: "nmcli --terse --fields ssid,signal dev wifi" ~=? scan scanCmd
testKnownCommand    = "Nmcli - List known wifi command" ~: "nmcli --terse --fields name con list"        ~=? scan knownCmd

testCleanStrings :: Test.HUnit.Test
testCleanStrings =
  TestList [ "testCleanString1" ~: "hello" ~=? clean quote "'hello'"
           , "testCleanString2" ~: "hello" ~=? clean quote "'hello"
           , "testCleanString3" ~: "hello" ~=? clean quote "hello"
           , "testCleanString4" ~: "hello'" ~=? clean quote "hello'"
           ]
  where quote = '\''

testConnectToWifiCommands :: Test.HUnit.Test
testConnectToWifiCommands =
  TestList [ "Nmcli - test connect to wifi command - with wifi" ~: "sudo nmcli con up id tatooine" ~=? connect conCmd "tatooine"
           , "Nmcli - test connect to wifi command - empty"     ~: "sudo nmcli con up id "         ~=? connect conCmd []
           ]


testElectWifis :: Test.HUnit.Test
testElectWifis =
  TestList ["Elect wifi - scanned wifi `intersect` known wifi == 'some-wifi-alone'" ~:
            Right "some-wifi-alone" ~=? unsafeElect (Right ["some-wifi-alone", "wifi2", "wifi3"]) (Right ["some-wifi-alone","known1", "known2"])
           ,"Elect wifi - no wifi available - no scanned wifi" ~:
            Left NoWifiAvailable ~=? unsafeElect (Right []) (Right ["some-wifi-alone","known1", "known2"])
           ,"Elect wifi - No wifi available - scanned wifi `intersect` known wifi == []" ~:
            Left NoWifiAvailable ~=? unsafeElect (Right ["wifi0"]) (Right ["some-wifi-alone"])
           ,"Elect wifi - Error - Error in scanned wifi so error is transmitted" ~:
            Left NoWifiAvailable ~=? unsafeElect (Left NoWifiAvailable) (Right ["some-wifi-alone"])
           ,"Elect wifi - Error - Error in known wifi list so error is transmitted  " ~:
            Left KnownWifiError ~=? unsafeElect (Right ["wifi0"]) (Left KnownWifiError)
  ]

testRuns :: Test.HUnit.Test
testRuns = TestList [ "run - Bad command - Empty command" ~:
                      TestCase $ run "" >>= assertEqual "Bad command - empty command" (Left $ BadCommand "")
                    , "run - Bad command - The command is badly formatted, which renders an error" ~:
                      TestCase $ run "ls -" >>= assertEqual "Bad command - incorrect command" (Left $ BadCommand "ls -")
                    , "run - Ok - retrieve the command's output result as a [String]" ~:
                      TestCase $ run "echo 'tatooine':75\n'myrkr':90" >>= assertEqual "Retrieve output from the command" (Right ["'tatooine':75","'myrkr':90"])
                    ]

testFormatMsgs :: Test.HUnit.Test
testFormatMsgs =
  TestList [ "testFormatMsg0" ~: ["No known wifi available!"] ~=? formatMsg "" id (Left NoWifiAvailable)
           , "testFormatMsg1" ~: [ "prefix string: "
                                 , "- input 0"
                                 , "- input 1"] ~=? formatMsg "prefix string: " ("- " ++) (Right ["input 0", "input 1"])
           ]

testSplits :: Test.HUnit.Test
testSplits =
  TestList [ "testSplit0" ~: split "\r\n" "a\r\nb\r\nd\r\ne" ~=? ["a","b","d","e"]
           , "testSplit1" ~: split "aaa"  "aaaXaaaXaaaXaaa"  ~=? ["","X","X","X",""]
           , "testSplit2" ~: split "x"    "x"                ~=? ["",""]
           ]

testAvailables :: Test.HUnit.Test
testAvailables = TestList [ "Retrieve the available wifi list." ~: do
                               (value, log) <- runWifiMonad $ available (Scan "echo 'tatooine':98\n'myrkr':100\n'arrakis':50")
                               assertEqual "Log should be"   ["Scanned wifi: \n","- myrkr","- tatooine","- arrakis"] log
                               assertEqual "value should be" (Right ["myrkr","tatooine","arrakis"]) value
                               return ()
                          , "A bad command is executed and caught then sent back" ~: do
                               (value, log) <- runWifiMonad $ available (Scan "bad-command")
                               assertEqual "Log should be"   ["'bad-command' is not a valid command."] log
                               assertEqual "value should be" (Left $ BadCommand "bad-command") value
                               return ()
                          ]

-- Full tests
tests :: Test.HUnit.Test
tests = TestList [ testCommandScanWifi
                 , testKnownCommand
                 , testCleanStrings
                 , testConnectToWifiCommands
                 , testElectWifis
                 , testRuns
                 , testFormatMsgs
                 , testSplits
                 , testAvailables
                 ]

main :: IO ()
main = runTestTT tests >>= print
