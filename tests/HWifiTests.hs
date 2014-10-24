module Main where

import           Network.HWifi          (createNewWifi, unsafeElect)
import           Network.Nmcli          (conCmd, createCmd, knownCmd, scanCmd)
import           Network.StandardPolicy (alreadyUsedWifis,
                                         alreadyUsedWifisWithLogs,
                                         availableWifis, availableWifisWithLogs,
                                         connectWifi, connectWifiWithLogs,
                                         createNewWifiConnectionAndConnect,
                                         createWifiWithLogs, scanAndConnectToKnownWifiWithMostPowerfulSignal)
import           Network.Types          (Command (..), CommandError (..))
import           Network.Utils          (clean, formatMsg, run, split)
import           Test.HUnit

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

testCreateWifiCommands :: Test.HUnit.Test
testCreateWifiCommands =
  TestList [ "Checkbox" ~: "sudo /usr/share/checkbox/scripts/create_connection wifi -S wpa -K some-pass myrkr" ~=? create createCmd "myrkr" "wpa" "some-pass"]

testElectWifis :: Test.HUnit.Test
testElectWifis =
  TestList ["Elect wifi - scanned wifi `intersect` known wifi == 'some-wifi'" ~:
            Right "some-wifi" ~=? unsafeElect (Right ["some-wifi", "wifi2", "wifi3"]) (Right ["wifi2", "some-wifi","known1", "known2"])
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

testAvailableWifiWithLogs :: Test.HUnit.Test
testAvailableWifiWithLogs = TestList [ "Retrieve the available wifi list." ~: do
                               (value, log) <- availableWifisWithLogs (Scan "echo 'tatooine':98\n'myrkr':100\n'arrakis':50")
                               assertEqual "Log should be"   ["Scanned wifi: \n","- myrkr","- tatooine","- arrakis"] log
                               assertEqual "Value should be" (Right ["myrkr","tatooine","arrakis"]) value
                               return ()
                          , "A bad command is executed and caught then sent back" ~: do
                               (value, log) <- availableWifisWithLogs (Scan "bad-command")
                               assertEqual "Log should be"   ["'bad-command' is not a valid command."] log
                               assertEqual "Value should be" (Left $ BadCommand "bad-command") value
                               return ()
                          ]

testAlreadyKnownWifiWithLogs :: Test.HUnit.Test
testAlreadyKnownWifiWithLogs = TestList [ "Retrieve the already known wifi." ~: do
                                  (value, log) <- alreadyUsedWifisWithLogs (Scan "echo tatooine\nmyrkr\narrakis")
                                  assertEqual "Log should be" ["\nAuto-connect wifi: \n","- tatooine","- myrkr","- arrakis"] log
                                  assertEqual "value should be" (Right ["tatooine", "myrkr","arrakis"]) value
                                  return ()
                             , "A bad command is executed and caught then sent back" ~: do
                                  (value, log) <- alreadyUsedWifisWithLogs (Scan "bad-command")
                                  assertEqual "Log should be"   ["'bad-command' is not a valid command."] log
                                  assertEqual "value should be" (Left $ BadCommand "bad-command") value
                                  return ()
                                  ]

testConnectWifiWithLogs :: Test.HUnit.Test
testConnectWifiWithLogs = TestList [ "Error is transmitted" ~: do
                                 (value, log) <- connectWifiWithLogs (Scan "not-used-command") (Left $ BadCommand "echo")
                                 assertEqual "Log should be" [] log
                                 assertEqual "value should be" (Left $ BadCommand "echo") value
                                 return ()
                            , "A wifi is provided and the connection should be ok" ~: do
                                 (value, log) <- connectWifiWithLogs (Connect ("echo connection " ++)) (Right "wifi-ssid")
                                 assertEqual "Log should be" ["\nConnection to wifi 'wifi-ssid'","connection wifi-ssid"] log
                                 assertEqual "value should be" (Right ["connection wifi-ssid"]) value
                                 return ()
                            , "Bad command is provided. This should break." ~: do
                                 (value, log) <- connectWifiWithLogs (Connect ("bad-command " ++)) (Right "wifi-ssid")
                                 assertEqual "Log should be" ["'bad-command wifi-ssid' is not a valid command."] log
                                 assertEqual "value should be" (Left $ BadCommand "bad-command wifi-ssid") value
                                 return ()
                            ]

testCreateWifiWithLogs :: Test.HUnit.Test
testCreateWifiWithLogs =
  TestList [ "A wifi is empty, so this should stop." ~: do
             (value, log) <- createWifiWithLogs createCmd "" "wifi-security" "psk"
             assertEqual "Log should be" [] log
             assertEqual "value should be" (Left $ EmptySSID "SSID must be specified!") value
             return ()
           , "A wifi is empty, so this should stop." ~: do
             (value, log) <- createWifiWithLogs fakeCreateCommand "ssid" "wpa" "psk"
             assertEqual "Log should be" ["\nCreation of the wifi connection 'ssid' and connection","ssid"] log
             assertEqual "value should be" (Right ["ssid"]) value
             return ()
           , "Bad command is provided. This should break." ~: do
             (value, log) <- createWifiWithLogs (fakeScanCommand "ssid") "ssid" "wpa" "psk"
             assertEqual "Log should be" [] log
             assertEqual "value should be" (Left $ BadCommand "Scan wifi") value
             return ()
           ]

testAvailables :: Test.HUnit.Test
testAvailables = TestList [ "Retrieve the available wifi list." ~: do
                               value <- availableWifis (Scan "echo 'tatooine':57\n'myrkr':40\n'arrakis':90")
                               assertEqual "Value should be" (Right ["arrakis","tatooine","myrkr"]) value
                               return ()
                          , "A bad command is executed and caught then sent back" ~: do
                               value <- availableWifis (Scan "bad-command")
                               assertEqual "Value should be" (Left $ BadCommand "bad-command") value
                               return ()
                          ]

testAlreadyKnowns :: Test.HUnit.Test
testAlreadyKnowns = TestList [ "Retrieve the already known wifi." ~: do
                                  value <- alreadyUsedWifis (Scan "echo myrkr\ntatooine\narrakis")
                                  assertEqual "value should be" (Right ["myrkr", "tatooine","arrakis"]) value
                                  return ()
                             , "A bad command is executed and caught then sent back" ~: do
                                  value <- alreadyUsedWifis (Scan "bad-command")
                                  assertEqual "value should be" (Left $ BadCommand "bad-command") value
                                  return ()
                                  ]

testConnectWifis :: Test.HUnit.Test
testConnectWifis = TestList [ "Error is transmitted" ~: do
                                 value <- connectWifi (Scan "not-used-command") (Left $ BadCommand "echo")
                                 assertEqual "value should be" (Left $ BadCommand "echo") value
                                 return ()
                            , "A wifi is provided and the connection should be ok" ~: do
                                 value <- connectWifi (Connect ("echo connection " ++)) (Right "wifi-ssid")
                                 assertEqual "value should be" (Right ["connection wifi-ssid"]) value
                                 return ()
                            , "Bad command is provided. This should break." ~: do
                                 value <- connectWifi (Connect ("bad-command " ++)) (Right "wifi-ssid")
                                 assertEqual "value should be" (Left $ BadCommand "bad-command wifi-ssid") value
                                 return ()
                            ]

fakeScanCommand, fakeAvailableCommand :: String -> Command
fakeScanCommand = Scan . ("echo " ++)
fakeAvailableCommand = Scan . ("echo " ++)

fakeConnectCommand :: Command
fakeConnectCommand = Connect ("echo " ++)

fakeCreateCommand :: Command
fakeCreateCommand = Create (\ si _ _ -> "echo " ++ si)

testScans :: Test.HUnit.Test
testScans = TestList [ "Ok - wifi elected - Only 'tatooine' is known so elected" ~: do
                         scanAndConnectToKnownWifiWithMostPowerfulSignal (fakeScanCommand "'tatooine':98\n'myrkr':100\n'arrakis':50")
                                                                         (fakeAvailableCommand "tatooine")
                                                                         fakeConnectCommand
                         return ()
                     , "Ok - wifi elected - Most powerful signal wifi 'myrkr' is elected" ~: do
                         scanAndConnectToKnownWifiWithMostPowerfulSignal (fakeScanCommand "'tatooine':90\n'myrkr':99\n'arrakis':50")
                                                                         (fakeAvailableCommand "tatooine\nmyrkr")
                                                                         fakeConnectCommand
                         return ()
                     , "Ok - No wifi elected - No scanned wifi" ~: do
                         scanAndConnectToKnownWifiWithMostPowerfulSignal (fakeScanCommand "")
                                                                         (fakeAvailableCommand "tatooine")
                                                                         fakeConnectCommand
                         return ()
                     , "Ok - No wifi elected - No known wifi" ~: do
                         scanAndConnectToKnownWifiWithMostPowerfulSignal (fakeScanCommand "'tatooine':90")
                                                                         (fakeAvailableCommand "myrkr")
                                                                         fakeConnectCommand
                         return ()
                     ]

-- Full tests
tests :: Test.HUnit.Test
tests = TestList [ testCommandScanWifi
                 , testKnownCommand
                 , testCleanStrings
                 , testConnectToWifiCommands
                 , testCreateWifiCommands
                 , testElectWifis
                 , testRuns
                 , testFormatMsgs
                 , testSplits
                 , testAvailables
                 , testAlreadyKnowns
                 , testConnectWifis
                 , testScans
                 , testAvailableWifiWithLogs
                 , testAlreadyKnownWifiWithLogs
                 , testConnectWifiWithLogs
                 , testScans
                 , testCreateWifiWithLogs
                 ]

main :: IO ()
main = runTestTT tests >>= print
