{-# LANGUAGE OverloadedStrings #-}

import Data.String
import Data.Maybe
import Data.ByteString.Char8 (unpack, readInt)
import qualified Data.ByteString.Char8 as S
import Test.HUnit
import URLAlert.Types
import URLAlert.SquidLog
import Data.Attoparsec (maybeResult)

s1 = "1330757555.727  43378 144.32.71.165 TCP_MISS/200 418 GET http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041 - DIRECT/69.171.227.51 text/plain"
s2 = "1330757555.720   9749 144.32.6.171 TCP_MISS/000 0 GET https://192.168.118.1:6789/ - DIRECT/192.168.118.1 -"
s3 = "1330757556.484 299937 144.32.45.222 TCP_MISS/200 2548 CONNECT www.facebook.com:443 - DIRECT/66.220.153.19 -"
s4 = "1330757555.720   9749 144.32.6.171 TCP_MISS/000 0 GET https://192.168.118.1/ - DIRECT/192.168.118.1 -"
s5 = "1330757556.566    141 144.32.12.76 TCP_MISS/200 3127 GET http://ib.adnxs.com/ab?enc=6PS8GwsKsz9q3PaYoyKxPwAAAAAAAOA_atz2mKMisT_o9LwbCwqzP7U-53Z1K14z61jvTu7-4yC0v1FPAAAAAKdLAABlAQAAbAEAAAIAAAD4XhQAPWQAAAEAAABVU0QAVVNEAKAAWALoYAcCUxYBAgUCAQQAAAAAFyVcLgAAAAA.&tt_code=vert-8&udj=uf%28%27a%27%2C+932%2C+1330757556%29%3Buf%28%27c%27%2C+259929%2C+1330757556%29%3Buf%28%27r%27%2C+1335032%2C+1330757556%29%3Bppv%2814414%2C+%273701443727251160757%27%2C+1330757556%2C+1333349556%2C+259929%2C+25661%2C+0%2C+0%2C+2592000%29%3Bppv%2828529%2C+%273701443727251160757%27%2C+1330757556%2C+1333349556%2C+259929%2C+25661%2C+0%2C+0%2C+2592000%29%3B&cnd=!pCQBYAjZ7g8Q-L1RGAAgvcgBMAA46MEBQABI7AJQp5cBWABggwFoAHAAeACAAbwliAGeBZABAZgBAaABAagBA7ABALkBUAmufAwIqD_BAVAJrnwMCKg_yQHDB5EStQbEP9kB4C2QoPgx5D_gAZJS&ccd=!swVrMAjZ7g8Q-L1RGL3IASAA&referrer=http://www.facebook.com&media_subtypes=1&dlo=1&pp=AAABNddU1_S7iKd4KE5B0AdSOzQySMA7C1ODPw&pubclick=http%3A%2F%2Fox-d.liftdna.com%2Fw%2F1.0%2Frc%3Fts%3D0c2lkPTk1Mzh8YXVpZD05OTI2OXxtcj0yMHxtdD1INHNJQUFBQUFBQUFBRjNNelFxQ1FCU0c0WHM1YTRVWnpfeTZibDJRUlVzWko0UEFhcGhVQlBIZU8yTWJpMW05SDgtY0dWNFRRdG5Ib2MzQURmWDlDaVdnTkVLZzA3azEzdWVDSzVzM3JSSjVvd3dyR3BUT2FBWXI3NkhraUV4TExhVmFsX2ltQTlYeFZGOE9lektCeWtvMGlkTnBMdWhsNEI5cFI4czFKdE5SOElJcG1jcEhxc0l5a2NMVkRmMmFZWFRkMEVLcDJKSTJfeVJ5cm5ZckNQRWZURnN3MVhIY2dDS0JzQVhoRjRndnVIV2JEZG15ZkFBY3Q2TDBLUUVBQUF8bXVpPTgwODYxYmEwLTg1ZWMtNDJlMy1hMjIyLWY1MTk5YWUxMmU1MXxhaWQ9MjQwMTF8bT0xfHB1Yj0xMjI0N3xsaWQ9MjAwMjV8YWk9NDA4YzlkZjgtODVmZS02ODkzLTQ5MzgtY2NiZmQyMDQ2MDFlfHQ9NHxtYz1VU0R8cmlkPTM1ODQ0M2E3LTk4Y2MtNDE2OS1iZTY0LWI2ODAyYjM1YTg3MHxibT18cGM9VVNEfHA9NjB8YWM9VVNEfHBtPVBSSUNJTkcuQ1BNfHJ0PTEzMzA3NTc1NTZ8cHI9NDB8YWR2PTEwMzk%26r%3D - DIRECT/68.67.179.247 text/javascript"
paramsS1 = "channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041"    
paramsS5 = "enc=6PS8GwsKsz9q3PaYoyKxPwAAAAAAAOA_atz2mKMisT_o9LwbCwqzP7U-53Z1K14z61jvTu7-4yC0v1FPAAAAAKdLAABlAQAAbAEAAAIAAAD4XhQAPWQAAAEAAABVU0QAVVNEAKAAWALoYAcCUxYBAgUCAQQAAAAAFyVcLgAAAAA.&tt_code=vert-8&udj=uf%28%27a%27%2C+932%2C+1330757556%29%3Buf%28%27c%27%2C+259929%2C+1330757556%29%3Buf%28%27r%27%2C+1335032%2C+1330757556%29%3Bppv%2814414%2C+%273701443727251160757%27%2C+1330757556%2C+1333349556%2C+259929%2C+25661%2C+0%2C+0%2C+2592000%29%3Bppv%2828529%2C+%273701443727251160757%27%2C+1330757556%2C+1333349556%2C+259929%2C+25661%2C+0%2C+0%2C+2592000%29%3B&cnd=!pCQBYAjZ7g8Q-L1RGAAgvcgBMAA46MEBQABI7AJQp5cBWABggwFoAHAAeACAAbwliAGeBZABAZgBAaABAagBA7ABALkBUAmufAwIqD_BAVAJrnwMCKg_yQHDB5EStQbEP9kB4C2QoPgx5D_gAZJS&ccd=!swVrMAjZ7g8Q-L1RGL3IASAA&referrer=http://www.facebook.com&media_subtypes=1&dlo=1&pp=AAABNddU1_S7iKd4KE5B0AdSOzQySMA7C1ODPw&pubclick=http%3A%2F%2Fox-d.liftdna.com%2Fw%2F1.0%2Frc%3Fts%3D0c2lkPTk1Mzh8YXVpZD05OTI2OXxtcj0yMHxtdD1INHNJQUFBQUFBQUFBRjNNelFxQ1FCU0c0WHM1YTRVWnpfeTZibDJRUlVzWko0UEFhcGhVQlBIZU8yTWJpMW05SDgtY0dWNFRRdG5Ib2MzQURmWDlDaVdnTkVLZzA3azEzdWVDSzVzM3JSSjVvd3dyR3BUT2FBWXI3NkhraUV4TExhVmFsX2ltQTlYeFZGOE9lektCeWtvMGlkTnBMdWhsNEI5cFI4czFKdE5SOElJcG1jcEhxc0l5a2NMVkRmMmFZWFRkMEVLcDJKSTJfeVJ5cm5ZckNQRWZURnN3MVhIY2dDS0JzQVhoRjRndnVIV2JEZG15ZkFBY3Q2TDBLUUVBQUF8bXVpPTgwODYxYmEwLTg1ZWMtNDJlMy1hMjIyLWY1MTk5YWUxMmU1MXxhaWQ9MjQwMTF8bT0xfHB1Yj0xMjI0N3xsaWQ9MjAwMjV8YWk9NDA4YzlkZjgtODVmZS02ODkzLTQ5MzgtY2NiZmQyMDQ2MDFlfHQ9NHxtYz1VU0R8cmlkPTM1ODQ0M2E3LTk4Y2MtNDE2OS1iZTY0LWI2ODAyYjM1YTg3MHxibT18cGM9VVNEfHA9NjB8YWM9VVNEfHBtPVBSSUNJTkcuQ1BNfHJ0PTEzMzA3NTc1NTZ8cHI9NDB8YWR2PTEwMzk%26r%3D"
x = "a line tha   should not parse"



testFn n v f s = TestCase $ assertEqual n v (getVal f s)
    where
      getVal p s = p $ fromJust (head $ parseLines s)

testFail s = TestCase $ assertEqual "fail" Nothing (head $ parseLines s::Maybe SquidLogLine)

testList = [testFn "ClientIP" "144.32.71.165"                clientIP          s1
            ,testFn "vhost"    "0-if-w.channel.facebook.com" (vhost . uri)     s1
            ,testFn "path"     "/pull"                       (uriPath . uri)   s1
            ,testFn "params"   paramsS1                      (uriParams . uri) s1
            ,testFn "port"     80                            (port . uri)      s1
            ,testFn "scheme"   HTTP                          (scheme . uri)    s1
            ,testFn "ClientIP" "144.32.6.171"                clientIP          s2
            ,testFn "vhost"    "192.168.118.1"               (vhost . uri)     s2
            ,testFn "path"     "/"                           (uriPath . uri)   s2
            ,testFn "port"     6789                          (port . uri)      s2
            ,testFn "scheme"   HTTPS                         (scheme . uri)    s2
            ,testFn "ClientIP" "144.32.45.222"               clientIP          s3
            ,testFn "vhost"    "www.facebook.com"            (vhost . uri)     s3
            ,testFn "path"     "/"                           (uriPath . uri)   s3
            ,testFn "params"   ""                            (uriParams. uri)  s3 
            ,testFn "port"     443                           (port. uri)       s3
            ,testFn "scheme"   HTTPS                         (scheme . uri)    s3
            ,testFn "port"     443                           (port . uri)      s4
            ,testFn "vhost"    "ib.adnxs.com"                (vhost . uri)     s5
            ,testFn "port"     80                            (port . uri)      s5
            ,testFn "path"     "/ab"                         (uriPath . uri)   s5
            ,testFn "params"   paramsS5                      (uriParams . uri) s5
            ,testFail x
           ]

main = runTestTT $ TestList testList
