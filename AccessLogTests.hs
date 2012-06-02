{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Maybe
import AccessLog
import Test.HUnit
import Data.Attoparsec.Text


s = "1330757555.727  43378 144.32.71.165 TCP_MISS/200 418 GET http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041 - DIRECT/69.171.227.51 text/plain\n"
urls = "http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041"    
x = "a line tha   should not parse" 

myTestCase t a v = doTestCase t a getValue v
    where
        getValue s p = p $ (fromJust . maybeResult $ parse accessLineParser s)
        doTestCase t a f v = TestCase $ assertEqual t a (f s v)

testTS         = myTestCase "TS is 1330757555.727"       1330757555.727  ts
testElapsed    = myTestCase "Elapsed time is 43378"       43378          elapsed
testClientIP   = myTestCase "Client IP is 144.32.71.165" "144.32.71.165" clientIP
testAction     = myTestCase "Action is TCP_MISS/200"     "TCP_MISS/200"  action
testSize       = myTestCase "Size is 418"                418             size
testMethod     = myTestCase "Method is GET"              "GET"           method
testURL        = myTestCase "URL should be right"        urls            url
testIdent      = myTestCase "Ident should be -"          "-"             ident
testHierarchy  = myTestCase "Hierarchy is Direct/69.171.227.51" "DIRECT/69.171.227.51" hierarchy
testMimeType   = myTestCase "Mime type is text/plain"    "text/plain"    mimeType
    
testFail = TestCase $ assertEqual "Invalid line should fail to parse" Nothing 
                (maybeResult $ parse accessLineParser x)

main = runTestTT $ TestList [testTS,       testElapsed,    testClientIP,
                             testAction,   testSize,       testMethod,      
                             testURL,      testIdent,      testHierarchy, 
                             testMimeType, testFail
                            ]
