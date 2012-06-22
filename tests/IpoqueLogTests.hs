{-# LANGUAGE OverloadedStrings #-}

import Data.String
import Data.Maybe
import Data.ByteString.Char8 (readInt, pack)
import Test.HUnit
import Data.Attoparsec.Char8
import URLAlert.Types
import URLAlert.IpoqueLog 


sb = "Jun  4 23:17:00 144.32.142.3 \"CampusEast2 - 144.32.142.3\"|host|144.32.34.125:60326|144.171.20.6:80|2011|06|04|23|17|00|\"www.nap.edu\"|"
s  = sb ++ "\"/images/footer_podicon.png\""
s2 = sb ++ "\"/\""
s3 = sb ++ "\"/f?q=2\""
s4 = sb ++ "\"/f?q=3&foo=bar\""
x = "Jun  4 23:17:00 144.32.143.3  "

doParse = parseOnly ipoqueLogLine

getVal p s = either (\x -> error "fail") p $ doParse s

testFn n v f s = TestCase $ assertEqual n v (getVal f (pack s))

testFail = TestCase $ assertEqual "Invalid line should fail to parse" Nothing 
                (maybeResult . parse ipoqueLogLine $ fromString x)

testList = [testFn "clientIP" "144.32.34.125"              clientIP s,
            testFn "vhost"    "www.nap.edu"                (vhost . uri) s,
            testFn "path"     "/images/footer_podicon.png" (uriPath  . uri) s,
            testFn "path"     "/"                          (uriPath . uri) s2,
            testFn "path"     "/f"                         (uriPath  . uri) s3,
            testFn "params"   ""                           (uriParams . uri) s,
            testFn "params"   ""                           (uriParams . uri) s2,
            testFn "params"   "q=2"                        (uriParams . uri) s3,
            testFn "params"   "q=3&foo=bar"                (uriParams . uri) s4,            
            testFn "port"     80                           (port . uri) s,
            testFn "scheme"   HTTP                         (scheme . uri) s,
            testFail
           ]

main = runTestTT $ TestList testList
