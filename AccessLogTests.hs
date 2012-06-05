{-# LANGUAGE OverloadedStrings #-}

import Data.String
import Data.Maybe
import Data.Either
import Data.ByteString.Char8 (unpack, readInt)
import Data.ByteString.Lazy
import AccessLog
import Test.HUnit
import Data.Attoparsec.ByteString


s = "1330757555.727  43378 144.32.71.165 TCP_MISS/200 418 GET http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041 - DIRECT/69.171.227.51 text/plain"
s2 = "1330757555.720   9749 144.32.6.171 TCP_MISS/000 0 GET http://192.168.118.1:6789/ - DIRECT/192.168.118.1 -"    
urls = "http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041"    
x = "a line tha   should not parse\n"


doParse s = parseOnly accessLogLine s

getInt s p = fst. fromJust . readInt $ r'
        where 
            r' = either (\x -> "fail") (\x -> p x) $ doParse s

getString s p = either (\x -> "fail") (\x -> p x) $ doParse s


testTS         = TestCase $ assertEqual "TS"           "1330757555.727" (getString s ts)
testElapsed    = TestCase $ assertEqual "Elapsed time" 43378           (getInt s elapsed)
testClientIP   = TestCase $ assertEqual "Client IP"    "144.32.71.165" (getString s clientIP)
testAction     = TestCase $ assertEqual "Action"       "TCP_MISS/200"  (getString s action)
testSize       = TestCase $ assertEqual "Size"         418             (getInt s size)
testMethod     = TestCase $ assertEqual "Method"       "GET"           (getString s method)
testURL        = TestCase $ assertEqual "URL"          urls            (getString s url)
testIdent      = TestCase $ assertEqual "Ident"        "-"             (getString s ident)
testMimeType   = TestCase $ assertEqual "Mime type"    "text/plain"    (getString s mimeType)
testHierarchy  = TestCase $ assertEqual "Hierarchy"    "DIRECT/69.171.227.51" (getString s hierarchy)

testFail = TestCase $ assertEqual "Invalid line should fail to parse" Nothing 
                (maybeResult . parse accessLogLine $ x)


main = runTestTT $ TestList [testTS,       testElapsed,    testClientIP,
                             testAction,   testSize,       testMethod,      
                             testURL,      testIdent,      testHierarchy, 
                             testMimeType, testFail
                            ]
