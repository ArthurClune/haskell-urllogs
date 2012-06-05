{-# LANGUAGE OverloadedStrings #-}

import Data.Text as T
import Data.Maybe
import IpoqueLog 
import Test.HUnit
import Data.Attoparsec.Text


s = "Jun  4 23:17:00 144.32.142.3 \"CampusEast2 - 144.32.142.3\"|host|144.32.34.125:60326|144.171.20.6:80|2011|06|04|23|17|00|\"www.nap.edu\"|\"/images/footer_podicon.png\"\n"
x = "Jun  4 23:17:00 144.32.143.3  "

-- t = text, a = correct answer, v = member of structure
myTestCase t a v = doTestCase t a getValue v
    where
        getValue s p = p $ (fromJust . maybeResult $ parse ipoqueLogLine s)
        doTestCase t a f v = TestCase $ assertEqual t a (f s v)

testDPort = myTestCase "Test dport is 80"       80               dport
testSport = myTestCase "Test sport is 60326"    60326            sport
testDate  = myTestCase "Date is 20110604231700" "20110604231700" date
testSrc   = myTestCase "Src is 144.32.34.125"   "144.32.34.125"  src
testDst   = myTestCase "Dst is 144.171.20.6"    "144.171.20.6"   dst
testVhost = myTestCase "vhost is www.nap.edu"   "www.nap.edu"    vhost
testUrl   = myTestCase "url is /images/footer_podicon.png" "/images/footer_podicon.png" url

testFail = TestCase $ assertEqual "Invalid line should fail to parse" Nothing 
                (maybeResult $ parse ipoqueLogLine x)

main = runTestTT $ TestList [testDPort, testSport, testDate,
                             testSrc,   testDst,   testVhost,
                             testUrl,   testFail ]
