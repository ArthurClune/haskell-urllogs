{-# LANGUAGE OverloadedStrings #-}

import Data.String
import Data.Maybe
import Data.ByteString.Char8 (readInt)
import IpoqueLog 
import Test.HUnit
import Data.Attoparsec.Char8


s = "Jun  4 23:17:00 144.32.142.3 \"CampusEast2 - 144.32.142.3\"|host|144.32.34.125:60326|144.171.20.6:80|2011|06|04|23|17|00|\"www.nap.edu\"|\"/images/footer_podicon.png\""
x = "Jun  4 23:17:00 144.32.143.3  "

doParse = parseOnly ipoqueLogLine

getVal p s = either (\x -> error("fail")) (\x -> p x) $ doParse s

testDPort = TestCase $ assertEqual "Dport" 80               (getVal dport s)
testSport = TestCase $ assertEqual "Sport" 60326            (getVal sport s)
testDate  = TestCase $ assertEqual "Date"  "20110604231700" (getVal date s)
testSrc   = TestCase $ assertEqual "Src"   "144.32.34.125"  (getVal src s)
testDst   = TestCase $ assertEqual "Dst"   "144.171.20.6"   (getVal dst s)
testVhost = TestCase $ assertEqual "vhost" "www.nap.edu"    (getVal vhost s)
testUrl   = TestCase $ assertEqual "url"   "/images/footer_podicon.png" (getVal url s)

testFail = TestCase $ assertEqual "Invalid line should fail to parse" Nothing 
                (maybeResult . parse ipoqueLogLine $ fromString x)


main = runTestTT $ TestList [testDPort, testSport, testDate,
                             testSrc,   testDst,   testVhost,
                             testUrl,   testFail ]
