module Futures where

import Contract

zcb :: Date -> Double -> Transfer -> Contract
zcb t x cur = time (at t) (scale (konst x) (one cur))

zcb1 = zcb (Date 3) 100 (Currency EUR)


--Returns recieving goods contract by specifying asset received and on what date by using a zcb contract
receiveGoods :: Date -> Double -> Transfer -> Contract
receiveGoods date quanitiy asset = get (zcb date quanitiy asset)
--Test by defining contract where 10 units of stock will be recieved in 10 units of time
receiveGoods1 = receiveGoods (Date 10) 10 (Asset Stocks)

--Returns contract where money of certain currency is sent on the specified date
sendMoney :: Date -> Double -> Transfer -> Contract
sendMoney date price cur = give (zcb date price cur)
--Test by defining contract where 100e is send in 10 units of time
sendMoney1 = sendMoney (Date 10) 100 (Currency EUR) 

--Futures contract returns a contract combining both recieveGoods and sendMoney 
futuresContract :: Date -> Double -> Transfer -> Double -> Transfer -> Contract
futuresContract sDate aQuanitity asset price cur = and' (receiveGoods sDate aQuanitity asset) (sendMoney sDate price cur)

futuresContract1 = futuresContract (Date 10) 1000 (Asset Bonds) 50 (Currency USD)
futuresContract2 = and' receiveGoods1 sendMoney1
