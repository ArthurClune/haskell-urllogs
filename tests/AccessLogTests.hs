{-# LANGUAGE OverloadedStrings #-}

import Data.String
import Data.Maybe
import Data.Either
import Data.ByteString.Char8 (unpack, readInt)
import Data.ByteString.Lazy
import AccessLog
import Test.HUnit
import Data.Attoparsec.ByteString
import Types

s = "1330757555.727  43378 144.32.71.165 TCP_MISS/200 418 GET http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041 - DIRECT/69.171.227.51 text/plain"
s2 = "1330757555.720   9749 144.32.6.171 TCP_MISS/000 0 GET http://192.168.118.1:6789/ - DIRECT/192.168.118.1 -"    
urls = "http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041"    
x = "a line tha   should not parse\n"


doParse s = parseOnly accessLogLine s

getVal p s = either (\x -> error "fail") (\x -> p x) $ doParse s


testClientIP   = TestCase $ assertEqual "Client IP"    "144.32.71.165" (getVal clientIP s)
testURL        = TestCase $ assertEqual "URL"          urls            (getVal url s)

testFail = TestCase $ assertEqual "Invalid line should fail to parse" Nothing 
                (maybeResult . parse accessLogLine $ x)


main = runTestTT $ TestList [testClientIP, testURL, testFail]
