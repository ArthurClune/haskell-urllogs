{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Data.ByteString.Lazy.Char8 (append)
import System.Exit
import Test.HUnit

import URLAlert.Types
import URLAlert.IpoqueLog 

sb = "Jun  4 23:17:00 192.168.142.3 \"CampusEast2 - 192.168.142.3\"|host|192.168.34.125:60326|144.171.20.6:80|2011|06|04|23|17|00|\"www.nap.edu\"|"
s  = sb `append` "\"/images/footer_podicon.png\""
s2 = sb `append` "\"/\""
s3 = sb `append` "\"/f?q=2\""
s4 = sb `append` "\"/f?q=3&foo=bar\""
x = "Jun  4 23:17:00 192.168.143.3  "

getVal p s = p $ fromJust (head $ parseLines s)
testFn n v f s = TestCase $ assertEqual n v (getVal f s)

testFail s = TestCase $ assertEqual "fail" Nothing (head $ parseLines s::Maybe IpoqueLogLine)

testList = [testFn  "clientIP" "192.168.34.125"              clientIP          s
            , testFn "vhost"    "www.nap.edu"                (vhost . uri)     s
            , testFn "path"     "/images/footer_podicon.png" (uriPath  . uri)  s
            , testFn "params"   ""                           (uriParams . uri) s
            , testFn "port"     80                           (port . uri)      s
            , testFn "scheme"   HTTP                         (scheme . uri)    s
            , testFn "path"     "/"                          (uriPath . uri)   s2
            , testFn "params"   ""                           (uriParams . uri) s2
            , testFn "path"     "/f"                         (uriPath  . uri)  s3
            , testFn "params"   "q=2"                        (uriParams . uri) s3
            , testFn "params"   "q=3&foo=bar"                (uriParams . uri) s4        
            , testFail x
           ]

main :: IO ()
main =
    do c <- runTestTT $ TestList testList
       if (errors c /= 0 || failures c /= 0) 
            then exitFailure
            else exitSuccess
