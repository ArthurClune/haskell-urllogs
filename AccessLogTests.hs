{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Maybe
import AccessLog
import Test.HUnit
import Data.Attoparsec.Text


s = "1330757555.727  43378 144.32.71.165 TCP_MISS/200 418 GET http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041 - DIRECT/69.171.227.51 text/plain\n"
s2 = "1330757555.720   9749 144.32.6.171 TCP_MISS/000 0 GET http://192.168.118.1:6789/ - DIRECT/192.168.118.1 -\n"    
urls = "http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041"    
x = "a line tha   should not parse\n"

myTestCase ln txt expct field = TestCase $ assertEqual txt expct (getValue ln field)
    where
        getValue ln field = field $ (fromJust . maybeResult $ parse accessLogLine ln)

testTS         = myTestCase s "TS"           1330757555.727  ts
testElapsed    = myTestCase s "Elapsed time" 43378           elapsed
testClientIP   = myTestCase s "Client IP"    "144.32.71.165" clientIP
testAction     = myTestCase s "Action"       "TCP_MISS/200"  action
testSize       = myTestCase s "Size"         418             size
testMethod     = myTestCase s "Method"       "GET"           method
testURL        = myTestCase s "URL"          urls            url
testIdent      = myTestCase s "Ident"        "-"             ident
testHierarchy  = myTestCase s "Hierarchy"    "DIRECT/69.171.227.51" hierarchy
testMimeType   = myTestCase s "Mime type"    "text/plain"    mimeType
    
testFail = TestCase $ assertEqual "Invalid line should fail to parse" Nothing 
                (maybeResult $ parse accessLogLine x)

main = runTestTT $ TestList [testTS,       testElapsed,    testClientIP,
                             testAction,   testSize,       testMethod,      
                             testURL,      testIdent,      testHierarchy, 
                             testMimeType, testFail
                            ]
