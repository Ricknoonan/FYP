module Betting where

import Contract

--This function returns type contract called time by combining a one contract, 
-- a scale contract and an odds contract. 

--bet :: Double -> Double -> Transfer -> Contract
--bet amount price cur = scale (konst (payout price amount)) (scale (konst amount) (one cur))

--sendMoney :: Date -> Double -> Double -> Transfer -> Contract
--sendMoney settleDate amount price cur  = time (at settleDate) (give (bet amount price cur))

--bettingContract = sendMoney 
--bettingContract1 = bettingContract (Date 10) 100 10 (Currency EUR)  

bet1 :: Double -> Double -> Transfer -> Integer -> Date -> Contract
bet1 amount price cur single settleDate = bet (time (at settleDate) (scale (konst (price)) (scale (konst amount) (one (cur)(single)))))

bet2 = bet1 100 5 (Currency EUR) 1 (Date 5) 
--Bet (Time (Obs False) (Scale (Obs 5) (Scale (Obs 100.0) (One (Currency EUR)))))


