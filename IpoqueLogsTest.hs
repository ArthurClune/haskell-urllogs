{-# LANGUAGE OverloadedStrings #-}

import Data.String
import Data.ByteString.Char8 (unpack, readInt)
import Data.Maybe
import IpoqueLogs 
import Test.HUnit
import Data.Attoparsec


s = "Jun  4 23:17:00 144.32.142.3 \"CampusEast2 - 144.32.142.3\"|host|144.32.34.125:60326|144.171.20.6:80|2011|06|04|23|17|00|\"www.nap.edu\"|\"/images/footer_podicon.png\""
x = "Jun  4 23:17:00 144.32.143.3  "

getInt s p = fst. fromJust . readInt $ p i
        where
            i = fromJust . maybeResult . parse ipoqueLineParser $ fromString s

getString s p = p $ (fromJust . maybeResult . parse ipoqueLineParser $ fromString s)

testDPort = TestCase $ assertEqual "Test dport is 80" 80 (getInt s dport)
testSport = TestCase $ assertEqual "Test sport is 60326" 60326 (getInt s sport)
testDate  = TestCase $ assertEqual "Date is 20110604231700" "20110604231700" (getString s date)
testSrc   = TestCase $ assertEqual "Src is 144.32.34.125"   "144.32.34.125"  (getString s src)
testDst   = TestCase $ assertEqual "Dst is 144.171.20.6"    "144.171.20.6"   (getString s dst)
testVhost = TestCase $ assertEqual "vhost is www.nap.edu"   "www.nap.edu"    (getString s vhost)
testUrl   = TestCase $ assertEqual "url is /images/footer_podicon.png" "/images/footer_podicon.png" (getString s url)

testFail = TestCase $ assertEqual "Invalid line should fail to parse" Nothing 
                (maybeResult . parse ipoqueLineParser $ fromString x)

main = runTestTT $ TestList [testDPort, testSport, testDate,
                             testSrc,   testDst,   testVhost,
                             testUrl,   testFail ]
