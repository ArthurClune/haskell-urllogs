{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString as S
import Data.Maybe
import System.Exit
import Test.HUnit

import URLAlert.Types
import URLAlert.SquidLog

s1 =  "1330757555.727  43378 192.168.71.165 TCP_MISS/200 418 GET http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041 - DIRECT/69.171.227.51 text/plain"
s2 =  "1330757555.720   9749 192.168.6.171 TCP_MISS/000 0 GET https://192.168.118.1:6789/ - DIRECT/192.168.118.1 -"
s3 =  "1330757556.484 299937 192.168.45.222 TCP_MISS/200 2548 CONNECT www.facebook.com:443 - DIRECT/66.220.153.19 -"
s4 =  "1330757555.720   9749 192.168.6.171 TCP_MISS/000 0 GET https://192.168.118.1/ - DIRECT/192.168.118.1 -"
s5 =  "1330757556.566    141 192.168.12.76 TCP_MISS/200 3127 GET http://ib.adnxs.com/ab?enc=6PS8GwsKsz9q3PaYoyKxPwAAAAAAAOA_atz2mKMisT_o9LwbCwqzP7U-53Z1K14z61jvTu7-4yC0v1FPAAAAAKdLAABlAQAAbAEAAAIAAAD4XhQAPWQAAAEAAABVU0QAVVNEAKAAWALoYAcCUxYBAgUCAQQAAAAAFyVcLgAAAAA.&tt_code=vert-8&udj=uf%28%27a%27%2C+932%2C+1330757556%29%3Buf%28%27c%27%2C+259929%2C+1330757556%29%3Buf%28%27r%27%2C+1335032%2C+1330757556%29%3Bppv%2814414%2C+%273701443727251160757%27%2C+1330757556%2C+1333349556%2C+259929%2C+25661%2C+0%2C+0%2C+2592000%29%3Bppv%2828529%2C+%273701443727251160757%27%2C+1330757556%2C+1333349556%2C+259929%2C+25661%2C+0%2C+0%2C+2592000%29%3B&cnd=!pCQBYAjZ7g8Q-L1RGAAgvcgBMAA46MEBQABI7AJQp5cBWABggwFoAHAAeACAAbwliAGeBZABAZgBAaABAagBA7ABALkBUAmufAwIqD_BAVAJrnwMCKg_yQHDB5EStQbEP9kB4C2QoPgx5D_gAZJS&ccd=!swVrMAjZ7g8Q-L1RGL3IASAA&referrer=http://www.facebook.com&media_subtypes=1&dlo=1&pp=AAABNddU1_S7iKd4KE5B0AdSOzQySMA7C1ODPw&pubclick=http%3A%2F%2Fox-d.liftdna.com%2Fw%2F1.0%2Frc%3Fts%3D0c2lkPTk1Mzh8YXVpZD05OTI2OXxtcj0yMHxtdD1INHNJQUFBQUFBQUFBRjNNelFxQ1FCU0c0WHM1YTRVWnpfeTZibDJRUlVzWko0UEFhcGhVQlBIZU8yTWJpMW05SDgtY0dWNFRRdG5Ib2MzQURmWDlDaVdnTkVLZzA3azEzdWVDSzVzM3JSSjVvd3dyR3BUT2FBWXI3NkhraUV4TExhVmFsX2ltQTlYeFZGOE9lektCeWtvMGlkTnBMdWhsNEI5cFI4czFKdE5SOElJcG1jcEhxc0l5a2NMVkRmMmFZWFRkMEVLcDJKSTJfeVJ5cm5ZckNQRWZURnN3MVhIY2dDS0JzQVhoRjRndnVIV2JEZG15ZkFBY3Q2TDBLUUVBQUF8bXVpPTgwODYxYmEwLTg1ZWMtNDJlMy1hMjIyLWY1MTk5YWUxMmU1MXxhaWQ9MjQwMTF8bT0xfHB1Yj0xMjI0N3xsaWQ9MjAwMjV8YWk9NDA4YzlkZjgtODVmZS02ODkzLTQ5MzgtY2NiZmQyMDQ2MDFlfHQ9NHxtYz1VU0R8cmlkPTM1ODQ0M2E3LTk4Y2MtNDE2OS1iZTY0LWI2ODAyYjM1YTg3MHxibT18cGM9VVNEfHA9NjB8YWM9VVNEfHBtPVBSSUNJTkcuQ1BNfHJ0PTEzMzA3NTc1NTZ8cHI9NDB8YWR2PTEwMzk%26r%3D - DIRECT/68.67.179.247 text/javascript"
s6 =  "1340204564.448      0 192.168.46.11 NONE/400 3194 NONE error:request-too-large - NONE/- text/html"
s7 =  "1340213512.257      0 192.168.34.117 NONE/400 3423 GET http://[fe80::215:99ff:fe4a:3d45%2513]/SmartPanel/sm.xml - NONE/- text/html"
s8 =  "1340213512.257      0 192.168.34.117 NONE/400 3423 GET http://[fe80::215:99ff:fe4a:3d45%2513]:456/SmartPanel/sm.xml?seq=2:3 - NONE/- text/html"
s9 =  "1340170913.689    303 192.168.76.207 TCP_MISS/200 1908 GET http://weather.service.msn.com/data.aspx?src=Windows7&wealocations=wc:UKXX0085&weadegreetype=F&culture=en-US - DIRECT/65.55.17.76 text/xml"
s10 = "1340170197.000  24773 192.168.206.236 TCP_MISS/503 3337 POST http://188.221.54.136/dwr/call/plaincall/nowPlayingService.getNowPlayingForCurrentPlayer.dwr - DIRECT/188.221.54.136 text/html"
s11 = "1340170709.466    962 192.168.55.139 TCP_MISS/200 91772 GET http://sophos.trinity.edu/staff/CIDs/S000/ESCOSX/cidsync.upd - DIRECT/131.194.151.84 All"
s12 = "1340171409.198      8 192.168.29.3 TCP_MISS/501 578 OPTIONS http://go.divx.com/ - DIRECT/2.22.228.43 text/html"
s13 = "1340176142.811    304 192.168.28.253 TCP_MISS/301 624 PROPFIND http://www.salixfinance.co.uk/downloads - DIRECT/64.90.56.89 text/html"
-- yes, this next one has really been seen in a log file
s14 = "1340179287.583    426 192.168.227.120 TCP_MISS/200 23846 GET http://www.barbadosrc.org/json_slide_list.php?directory=slides - DIRECT/209.203.226.167 {$content_type}"
s15 = "1340181298.947    320 192.168.2.123 TCP_MISS/200 857 POST http://check.sanasecurity.com - DIRECT/204.193.144.31 text/html"
-- another genuine line from within a logfile. We've going to fail on this one
s16 = "1340189618.271      0 192.168.0.35 NONE/400 11544 GET http://pong.qubitproducts.com/m?guid=h3oa8k657rdpia9&ti=missselfridge&wst=6382&cid=1340189495962.988706&i=38&m=c3d8Njc0fDR8NDI5fDEzMDI1fGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdL3VsW0BjbGFzcz0icHJvZHVjdCJdL2xpW0BjbGFzcz0id2FzX3ByaWNlIHByb2R1Y3RfcHJpY2UiXXwxMzAxN3wzMzh8MTcwfDE2fDcsc3d8Njc0fDB8NDI5fDEyODkxfGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdL3VsW0BjbGFzcz0icHJvZHVjdCJdL2xpW0BjbGFzcz0icHJvZHVjdF9pbWFnZSJdL2FbMV0vaW1nWzFdfDEyNzMwfDMzOHwxNzV8MjM4fDE2LHN3fDY3NXwwfDQyOXwxMjc2NnxpZCgid3JhcHBlcl9wYWdlX2NvbnRlbnQiKS9kaXZbQGNsYXNzPSJ3cmFwcGVyX3Byb2R1Y3RfbGlzdCBjZiBjb2xzXzQiXS9kaXZbQGNsYXNzPSJzcF81IGJsb2NrXzEiXS91bFtAY2xhc3M9InByb2R1Y3QiXS9saVtAY2xhc3M9InByb2R1Y3RfaW1hZ2UiXS9hWzFdL2ltZ1sxXXwxMjczMHwzMzh8MTc1fDIzOHwxMSxzd3w2NzV8MHw0Mjl8MTI2MTN8aWQoIndyYXBwZXJfcGFnZV9jb250ZW50IikvZGl2W0BjbGFzcz0id3JhcHBlcl9wcm9kdWN0X2xpc3QgY2YgY29sc180Il0vZGl2W0BjbGFzcz0ic3BfNSBibG9ja18xIl0vdWxbQGNsYXNzPSJwcm9kdWN0Il0vbGlbQGNsYXNzPSJwcm9kdWN0X2ltYWdlIl0vYVsxXS9pbWdbMV18MTI3MzB8MzM4fDE3NXwyMzh8MTEsc3d8Njc1fDB8NDI5fDEyNDU1fGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdL3VsW0BjbGFzcz0icHJvZHVjdCJdL2xpW0BjbGFzcz0icHJvZHVjdF9pbWFnZSJdL2FbMV0vaW1nWzFdfDEyNzMwfDMzOHwxNzV8MjM4fDE0LHN3fDY3NXwwfDQyOXwxMjI2MnxpZCgid3JhcHBlcl9wYWdlX2NvbnRlbnQiKS9kaXZbQGNsYXNzPSJ3cmFwcGVyX3Byb2R1Y3RfbGlzdCBjZiBjb2xzXzQiXS9kaXZbQGNsYXNzPSJzcF81IGJsb2NrXzEiXS91bFtAY2xhc3M9InByb2R1Y3QiXS9saVtAY2xhc3M9InByb2R1Y3RfaW1hZ2UiXS9hWzFdL2ltZ1sxXXwxMjczMHwzMzh8MTc1fDIzOHw3LHN3fDY3NXwwfDQyOXwxMjE2OHxpZCgid3JhcHBlcl9wYWdlX2NvbnRlbnQiKS9kaXZbQGNsYXNzPSJ3cmFwcGVyX3Byb2R1Y3RfbGlzdCBjZiBjb2xzXzQiXS9kaXZbQGNsYXNzPSJzcF81IGJsb2NrXzEiXS91bFtAY2xhc3M9InByb2R1Y3QiXS9saVtAY2xhc3M9InByb2R1Y3RfaW1hZ2UiXS9hWzFdL2ltZ1sxXXwxMjczMHwzMzh8MTc1fDIzOHwxMyxzd3w2NzZ8MHw0Mjl8MTE5MDd8aWQoIndyYXBwZXJfcGFnZV9jb250ZW50IikvZGl2W0BjbGFzcz0id3JhcHBlcl9wcm9kdWN0X2xpc3QgY2YgY29sc180Il0vZGl2W0BjbGFzcz0ic3BfNSBibG9ja18xIl0vdWxbQGNsYXNzPSJwcm9kdWN0Il0vbGlbQGNsYXNzPSJwcm9kdWN0X2ltYWdlIl0vYVsxXS9pbWdbMV18MTI3MzB8MzM4fDE3NXwyMzh8MjQsc3d8Njc2fDB8NDI5fDExNjcwfGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdL3VsW0BjbGFzcz0icHJvZHVjdCJdL2xpW0BjbGFzcz0icHJvZHVjdF9pbWFnZSJdL2FbMV0vaW1nWzFdfDEyNzMwfDMzOHwxNzV8MjM4fDExLHN3fDY3NnwwfDQyOXwxMTUxN3xpZCgid3JhcHBlcl9wYWdlX2NvbnRlbnQiKS9kaXZbQGNsYXNzPSJ3cmFwcGVyX3Byb2R1Y3RfbGlzdCBjZiBjb2xzXzQiXS9kaXZbQGNsYXNzPSJzcF81IGJsb2NrXzEiXS91bFtAY2xhc3M9InByb2R1Y3QiXS9saVtAY2xhc3M9InByb2R1Y3RfaW1hZ2UiXS9hWzFdL2ltZ1sxXXwxMjczMHwzMzh8MTc1fDIzOHwxMSxzd3w2NzZ8MHw0Mjl8MTEzNjd8aWQoIndyYXBwZXJfcGFnZV9jb250ZW50IikvZGl2W0BjbGFzcz0id3JhcHBlcl9wcm9kdWN0X2xpc3QgY2YgY29sc180Il0vZGl2W0BjbGFzcz0ic3BfNSBibG9ja18xIl0vdWxbQGNsYXNzPSJwcm9kdWN0Il0vbGlbQGNsYXNzPSJwcm9kdWN0X2ltYWdlIl0vYVsxXS9pbWdbMV18MTI3MzB8MzM4fDE3NXwyMzh8MTAsc3d8Njc2fDB8NDI5fDExMjI5fGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdL3VsW0BjbGFzcz0icHJvZHVjdCJdL2xpW0BjbGFzcz0icHJvZHVjdF9pbWFnZSJdL2FbMV0vaW1nWzFdfDEyNzMwfDMzOHwxNzV8MjM4fDksc3d8Njc2fDB8NDI5fDExMTAzfGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdL3VsW0BjbGFzcz0icHJvZHVjdCJdL2xpW0BjbGFzcz0icHJvZHVjdF9pbWFnZSJdL2FbMV0vaW1nWzFdfDEyNzMwfDMzOHwxNzV8MjM4fDgsc3d8Njc3fDB8NDI5fDEwOTg3fGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdL3VsW0BjbGFzcz0icHJvZHVjdCJdL2xpW0BjbGFzcz0icHJvZHVjdF9pbWFnZSJdL2FbMV0vaW1nWzFdfDEyNzMwfDMzOHwxNzV8MjM4fDgsc3d8Njc3fDB8NDI5fDEwODc5fGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdL3VsW0BjbGFzcz0icHJvZHVjdCJdL2xpW0BjbGFzcz0icHJvZHVjdF9pbWFnZSJdL2FbMV0vaW1nWzFdfDEyNzMwfDMzOHwxNzV8MjM4fDcsc3d8Njc3fDB8NDI5fDEwNzc3fGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdL3VsW0BjbGFzcz0icHJvZHVjdCJdL2xpW0BjbGFzcz0icHJvZHVjdF9pbWFnZSJdL2FbMV0vaW1nWzFdfDEyNzMwfDMzOHwxNzV8MjM4fDcsc3d8Njc3fDB8NDI5fDEwNjc5fGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdL3VsW0BjbGFzcz0icHJvZHVjdCJdL2xpW0BjbGFzcz0icHJvZHVjdF9pbWFnZSJdL2FbMV0vaW1nWzFdfDEyNzMwfDMzOHwxNzV8MjM4fDcsc3d8Njc3fDB8NDI5fDEwNDk3fGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdL3VsW0BjbGFzcz0icHJvZHVjdCJdL2xpW0BjbGFzcz0icHJvZHVjdF9pbWFnZSJdL2FbMV0vaW1nWzFdfDEyNzMwfDMzOHwxNzV8MjM4fDEyLHN3fDY3OHwwfDQyOXwxMDQxMnxpZCgid3JhcHBlcl9wYWdlX2NvbnRlbnQiKS9kaXZbQGNsYXNzPSJ3cmFwcGVyX3Byb2R1Y3RfbGlzdCBjZiBjb2xzXzQiXS9kaXZbQGNsYXNzPSJzcF81IGJsb2NrXzEiXS91bFtAY2xhc3M9InByb2R1Y3QiXS9saVtAY2xhc3M9InByb2R1Y3RfaW1hZ2UiXS9hWzFdL2ltZ1sxXXwxMjczMHwzMzh8MTc1fDIzOHw5LHN3fDY3OHwwfDQyOXwxMDIyNXxpZCgid3JhcHBlcl9wYWdlX2NvbnRlbnQiKS9kaXZbQGNsYXNzPSJ3cmFwcGVyX3Byb2R1Y3RfbGlzdCBjZiBjb2xzXzQiXS9kaXZbQGNsYXNzPSJzcF81IGJsb2NrXzEiXS91bFtAY2xhc3M9InByb2R1Y3QiXS9saVtAY2xhc3M9InByb2R1Y3RfaW1hZ2UiXS9hWzFdL2ltZ1sxXXwxMjczMHwzMzh8MTc1fDIzOHw5LHN3fDY3OHwwfDQyOXwxMDEwMnxpZCgid3JhcHBlcl9wYWdlX2NvbnRlbnQiKS9kaXZbQGNsYXNzPSJ3cmFwcGVyX3Byb2R1Y3RfbGlzdCBjZiBjb2xzXzQiXS9kaXZbQGNsYXNzPSJzcF81IGJsb2NrXzEiXS91bFtAY2xhc3M9InByb2R1Y3QiXS9saVtAY2xhc3M9InByb2R1Y3RfaW1hZ2UiXS9hWzFdL2ltZ1sxXXwxMjczMHwzMzh8MTc1fDIzOHw4LHN3fDY3OXwwfDQyOXw5OTkyfGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdL3VsW0BjbGFzcz0icHJvZHVjdCJdL2xpW0BjbGFzcz0icHJvZHVjdF9pbWFnZSJdL2FbMV0vaW1nWzFdfDEyNzMwfDMzOHwxNzV8MjM4fDcsc3d8Njc5fDB8NDI5fDk4OTJ8aWQoIndyYXBwZXJfcGFnZV9jb250ZW50IikvZGl2W0BjbGFzcz0id3JhcHBlcl9wcm9kdWN0X2xpc3QgY2YgY29sc180Il0vZGl2W0BjbGFzcz0ic3BfNSBibG9ja18xIl0vdWxbQGNsYXNzPSJwcm9kdWN0Il0vbGlbQGNsYXNzPSJwcm9kdWN0X2ltYWdlIl0vYVsxXS9pbWdbMV18MTI3MzB8MzM4fDE3NXwyMzh8Nyxzd3w2Nzl8MXw0Mjl8OTc2NHxpZCgid3JhcHBlcl9wYWdlX2NvbnRlbnQiKS9kaXZbQGNsYXNzPSJ3cmFwcGVyX3Byb2R1Y3RfbGlzdCBjZiBjb2xzXzQiXS9kaXZbQGNsYXNzPSJzcF81IGJsb2NrXzEiXS91bFtAY2xhc3M9InByb2R1Y3QiXS9saVtAY2xhc3M9InByb2R1Y3RfaW1hZ2UiXS9hWzFdL2ltZ1sxXXwxMjczMHwzMzh8MTc1fDIzOHw4LHN3fDY4MHwxfDQyOXw5NjU2fGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdL3VsW0BjbGFzcz0icHJvZHVjdCJdL2xpW0BjbGFzcz0icHJvZHVjdF9pbWFnZSJdL2FbMV0vaW1nWzFdfDEyNzMwfDMzOHwxNzV8MjM4fDcsc3d8NjgxfDF8NDI5fDk1NDN8aWQoIndyYXBwZXJfcGFnZV9jb250ZW50IikvZGl2W0BjbGFzcz0id3JhcHBlcl9wcm9kdWN0X2xpc3QgY2YgY29sc180Il0vZGl2W0BjbGFzcz0ic3BfNSBibG9ja18xIl0vdWxbQGNsYXNzPSJwcm9kdWN0Il0vbGlbQGNsYXNzPSJwcm9kdWN0X2ltYWdlIl0vYVsxXS9pbWdbMV18MTI3MzB8MzM4fDE3NXwyMzh8Nyxzd3w2ODJ8MXw0Mjl8OTQ0M3xpZCgid3JhcHBlcl9wYWdlX2NvbnRlbnQiKS9kaXZbQGNsYXNzPSJ3cmFwcGVyX3Byb2R1Y3RfbGlzdCBjZiBjb2xzXzQiXS9kaXZbQGNsYXNzPSJzcF81IGJsb2NrXzEiXS91bFtAY2xhc3M9InByb2R1Y3QiXS9saVtAY2xhc3M9InByb2R1Y3RfaW1hZ2UiXS9hWzFdL2ltZ1sxXXwxMjczMHwzMzh8MTc1fDIzOHw3LHN3fDY4NHwyfDQyOXw5MzQwfGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdL3VsW0BjbGFzcz0icHJvZHVjdCJdL2xpW0BjbGFzcz0icHJvZHVjdF9pbWFnZSJdL2FbMV0vaW1nWzFdfDEyNzMwfDMzOHwxNzV8MjM4fDcsc3d8Njg4fDR8NDI5fDkyNjN8aWQoIndyYXBwZXJfcGFnZV9jb250ZW50IikvZGl2W0BjbGFzcz0id3JhcHBlcl9wcm9kdWN0X2xpc3QgY2YgY29sc180Il0vZGl2W0BjbGFzcz0ic3BfNSBibG9ja18xIl0vdWxbQGNsYXNzPSJwcm9kdWN0Il0vbGlbQGNsYXNzPSJwcm9kdWN0X2ltYWdlIl0vYVsxXS9pbWdbMV18OTE4NXwzMzh8MTc1fDIzOHw3LHN3fDY4OHwwfDQyOXw5MTY4fGlkKCJ3cmFwcGVyX3BhZ2VfY29udGVudCIpL2RpdltAY2xhc3M9IndyYXBwZXJfcHJvZHVjdF9saXN0IGNmIGNvbHNfNCJdL2RpdltAY2xhc3M9InNwXzUgYmxvY2tfMSJdfDkxNjV8MzI1fDE5OHwzMzB8MTUsc3d8Njg4fDB8NDI5fDkwMDZ8aWQoIndyYXBwZXJfcGFnZV9jb250ZW50IikvZGl2W0BjbGFzcz0id3JhcHBlcl9wcm9kdWN0X2xpc3QgY2YgY29sc180Il0vZGl2W0BjbGFzcz0ic3BfNSBibG9ja18xIl0vdWxbQGNsYXNzPSJwcm9kdWN0Il0vbGlbQGN"

paramsS1 = "channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041"    
paramsS5 = "enc=6PS8GwsKsz9q3PaYoyKxPwAAAAAAAOA_atz2mKMisT_o9LwbCwqzP7U-53Z1K14z61jvTu7-4yC0v1FPAAAAAKdLAABlAQAAbAEAAAIAAAD4XhQAPWQAAAEAAABVU0QAVVNEAKAAWALoYAcCUxYBAgUCAQQAAAAAFyVcLgAAAAA.&tt_code=vert-8&udj=uf%28%27a%27%2C+932%2C+1330757556%29%3Buf%28%27c%27%2C+259929%2C+1330757556%29%3Buf%28%27r%27%2C+1335032%2C+1330757556%29%3Bppv%2814414%2C+%273701443727251160757%27%2C+1330757556%2C+1333349556%2C+259929%2C+25661%2C+0%2C+0%2C+2592000%29%3Bppv%2828529%2C+%273701443727251160757%27%2C+1330757556%2C+1333349556%2C+259929%2C+25661%2C+0%2C+0%2C+2592000%29%3B&cnd=!pCQBYAjZ7g8Q-L1RGAAgvcgBMAA46MEBQABI7AJQp5cBWABggwFoAHAAeACAAbwliAGeBZABAZgBAaABAagBA7ABALkBUAmufAwIqD_BAVAJrnwMCKg_yQHDB5EStQbEP9kB4C2QoPgx5D_gAZJS&ccd=!swVrMAjZ7g8Q-L1RGL3IASAA&referrer=http://www.facebook.com&media_subtypes=1&dlo=1&pp=AAABNddU1_S7iKd4KE5B0AdSOzQySMA7C1ODPw&pubclick=http%3A%2F%2Fox-d.liftdna.com%2Fw%2F1.0%2Frc%3Fts%3D0c2lkPTk1Mzh8YXVpZD05OTI2OXxtcj0yMHxtdD1INHNJQUFBQUFBQUFBRjNNelFxQ1FCU0c0WHM1YTRVWnpfeTZibDJRUlVzWko0UEFhcGhVQlBIZU8yTWJpMW05SDgtY0dWNFRRdG5Ib2MzQURmWDlDaVdnTkVLZzA3azEzdWVDSzVzM3JSSjVvd3dyR3BUT2FBWXI3NkhraUV4TExhVmFsX2ltQTlYeFZGOE9lektCeWtvMGlkTnBMdWhsNEI5cFI4czFKdE5SOElJcG1jcEhxc0l5a2NMVkRmMmFZWFRkMEVLcDJKSTJfeVJ5cm5ZckNQRWZURnN3MVhIY2dDS0JzQVhoRjRndnVIV2JEZG15ZkFBY3Q2TDBLUUVBQUF8bXVpPTgwODYxYmEwLTg1ZWMtNDJlMy1hMjIyLWY1MTk5YWUxMmU1MXxhaWQ9MjQwMTF8bT0xfHB1Yj0xMjI0N3xsaWQ9MjAwMjV8YWk9NDA4YzlkZjgtODVmZS02ODkzLTQ5MzgtY2NiZmQyMDQ2MDFlfHQ9NHxtYz1VU0R8cmlkPTM1ODQ0M2E3LTk4Y2MtNDE2OS1iZTY0LWI2ODAyYjM1YTg3MHxibT18cGM9VVNEfHA9NjB8YWM9VVNEfHBtPVBSSUNJTkcuQ1BNfHJ0PTEzMzA3NTc1NTZ8cHI9NDB8YWR2PTEwMzk%26r%3D"
paramsS9 = "src=Windows7&wealocations=wc:UKXX0085&weadegreetype=F&culture=en-US"

testFn n v f s = TestCase $ assertEqual n v (getVal f s)
    where
      getVal p s = p $ fromJust (runParse s)

testFail s = TestCase $ assertEqual "fail" Nothing (runParse s)

testList = [testFn "elapsed s1"      43378                         elapsed           s1
            ,testFn "timestamp s1"   1330757555                    ts                s1
            ,testFn "clientIP s1"    "192.168.71.165"              clientIP          s1
            ,testFn "action s1"      "TCP_MISS"                    action            s1
            ,testFn "code s1"        200                           resultCode        s1
            ,testFn "size s1"        418                           size              s1
            ,testFn "method s1"      GET                           method            s1
            ,testFn "ident s1"       "-a"                           ident             s1
            ,testFn "hierarchy s1"   "DIRECT"                      hierarchy         s1
            ,testFn "remote ip s1"   "69.171.227.51"               remIP             s1
            ,testFn "mimetype s1"    "text/plain"                  mimeType          s1
            ,testFn "vhost s1"       "0-if-w.channel.facebook.com" (vhost . uri)     s1
            ,testFn "path s1"        "/pull"                       (uriPath . uri)   s1
            ,testFn "params s1"      paramsS1                      (uriParams . uri) s1
            ,testFn "port s1"        80                            (port . uri)      s1
            ,testFn "scheme s1"      HTTP                          (scheme . uri)    s1
            ,testFn "clientIP s2"    "192.168.6.171"               clientIP          s2
            ,testFn "vhost s2"       "192.168.118.1"               (vhost . uri)     s2
            ,testFn "path s2"        "/"                           (uriPath . uri)   s2
            ,testFn "port s2"        6789                          (port . uri)      s2
            ,testFn "scheme s2"      HTTPS                         (scheme . uri)    s2
            ,testFn "clientIP s3"    "192.168.45.222"              clientIP          s3
            ,testFn "vhost s3"       "www.facebook.com"            (vhost . uri)     s3
            ,testFn "path s3"        "/"                           (uriPath . uri)   s3
            ,testFn "params s3"      ""                            (uriParams. uri)  s3 
            ,testFn "port s3"        443                           (port. uri)       s3
            ,testFn "scheme s3"      HTTPS                         (scheme . uri)    s3
            ,testFn "port s4"        443                           (port . uri)      s4
            ,testFn "vhost s5"       "ib.adnxs.com"                (vhost . uri)     s5
            ,testFn "port s5"        80                            (port . uri)      s5
            ,testFn "path s5"        "/ab"                         (uriPath . uri)   s5
            ,testFn "params s5"      paramsS5                      (uriParams . uri) s5
            ,testFn "scheme s6"      NONE                          (scheme . uri)    s6
            ,testFn "method s6"      MNONE                         method            s6
            ,testFn "elapsed s6"     0                             elapsed           s6
            ,testFn "vhost s6"       "error:request-too-large"     (vhost . uri)     s6
            ,testFn "vhost s7"       "fe80::215:99ff:fe4a:3d45%2513" (vhost . uri)   s7
            ,testFn "port s7"        80                            (port . uri)      s7
            ,testFn "path s7"        "/SmartPanel/sm.xml"          (uriPath . uri)   s7
            ,testFn "port s8"        456                           (port . uri)      s8
            ,testFn "path s8"        "/SmartPanel/sm.xml"          (uriPath . uri)   s8
            ,testFn "params s8"      "seq=2:3"                     (uriParams . uri) s8
            ,testFn "vhost s9"       "weather.service.msn.com"     (vhost . uri)     s9
            ,testFn "path s9"        "/data.aspx"                  (uriPath . uri)   s9
            ,testFn "params s9"      paramsS9                      (uriParams . uri) s9
            ,testFn "method s10"     POST                          method            s10
            ,testFn "mimetype s11"   "All"                         mimeType          s11
            ,testFn "method s12"     OPTIONS                       method            s12
            ,testFn "method s13"     PROPFIND                      method            s13
            ,testFn "mimetype s14"   "{$content_type}"             mimeType          s14
            ,testFn "vhost s15"      "check.sanasecurity.com"      (vhost . uri)     s15
            ,testFail s16
           ]

main :: IO ()
main =
    do c <- runTestTT $ TestList testList
       if (errors c /= 0 || failures c /= 0) 
            then exitFailure
            else exitSuccess
