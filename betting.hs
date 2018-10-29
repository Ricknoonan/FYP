module Betting where

import Contract

--This function returns type contract called time by combining a one contract, 
-- a scale contract and an odds contract. 

bet :: Double -> Double -> Transfer -> Contract
bet amount price cur = scale (konst (payout price amount)) (scale (konst amount) (one cur))

sendMoney :: Date -> Double -> Double -> Transfer -> Contract
sendMoney settleDate amount price cur  = time (at settleDate) (give (bet amount price cur))

bettingContract = sendMoney 
bettingContract1 = bettingContract (Date 10) 100 10 (Currency EUR)  
