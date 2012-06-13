{-# LANGUAGE OverloadedStrings #-}

import Data.String
import Data.Maybe
import Data.ByteString.Char8 (readInt)
import IpoqueLog 
import Test.HUnit
import Data.Attoparsec.Char8
import Types

s = "Jun  4 23:17:00 144.32.142.3 \"CampusEast2 - 144.32.142.3\"|host|144.32.34.125:60326|144.171.20.6:80|2011|06|04|23|17|00|\"www.nap.edu\"|\"/images/footer_podicon.png\""
x = "Jun  4 23:17:00 144.32.143.3  "

doParse = parseOnly ipoqueLogLine

getVal p s = either (\x -> error("fail")) (\x -> p x) $ doParse s

testSrc   = TestCase $ assertEqual "clientIP"   "144.32.34.125"  (getVal clientIP s)
testUrl   = TestCase $ assertEqual "url"   "www.nap.edu/images/footer_podicon.png" (getVal url s)

testFail = TestCase $ assertEqual "Invalid line should fail to parse" Nothing 
                (maybeResult . parse ipoqueLogLine $ fromString x)


main = runTestTT $ TestList [testSrc, testUrl, testFail ]
