module IpoqueLogsTest
where

{-# LANGUAGE OverloadedStrings #-}

import Data.String
import Data.ByteString.Char8 (unpack)
import Data.Maybe
import IpoqueLogs 
import Test.HUnit
import Data.Attoparsec


s = "Jun  4 23:17:00 144.32.142.3 \"CampusEast2 - 144.32.142.3\"|host|144.32.34.125:60326|144.171.20.6:80|2011|06|04|23|17|00|\"www.nap.edu\"|\"/images/footer_podicon.png\""

testBar = TestCase $ assertEqual "Test dport is 80" "80"  (unpack $ dport pl)
        where
            pl = fromJust . maybeResult . parse ipoqueLineParser $ fromString s

main = runTestTT testBar

--main = print . maybeResult . parse ipoqueLineParser $ fromString s