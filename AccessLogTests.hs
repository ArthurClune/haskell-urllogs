{-# LANGUAGE OverloadedStrings #-}

import Data.String
import Data.ByteString.Char8 (unpack, readInt)
import Data.Maybe
import AccessLog
import Test.HUnit
import Data.Attoparsec


s = "1330757555.727  43378 144.32.71.165 TCP_MISS/200 418 GET http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041 - DIRECT/69.171.227.51 text/plain"
urls = "http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041"    
x = "a line that should not parse"

getInt s p = fst. fromJust . readInt $ p i
        where
            i = fromJust . maybeResult . parse accessLineParser $ fromString s

getString s p = p $ (fromJust . maybeResult . parse accessLineParser $ fromString s)

testTS       = TestCase $ assertEqual "TS is 1330757555.727"  "1330757555.727"      (getString s ts)
testBytes    = TestCase $ assertEqual "Bytes are 43378"       43378                 (getInt s bytes)
testClientIP = TestCase $ assertEqual "Client IP is 144.32.71.165" "144.32.71.165"  (getString s clientIP)
testURL      = TestCase $ assertEqual "URL should be right"   urls                  (getString s url)
testConnType = TestCase $ assertEqual "ConnType is Direct"    "DIRECT"              (getString s connType)
testServerIP = TestCase $ assertEqual "ServerIP is 69.171.227.51" "69.171.227.51"   (getString s serverIP)
testMimeType = TestCase $ assertEqual "Mime type is test/html" "test/html"          (getString s mimeType)
    
testFail = TestCase $ assertEqual "Invalid line should fail to parse" Nothing 
                (maybeResult . parse accessLineParser $ fromString x)

main = runTestTT $ TestList [testTS,       testBytes,    testClientIP,
                             testURL,      testConnType, testServerIP,
                             testMimeType, testFail
                            ]
