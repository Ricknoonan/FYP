module Betting where

import Contract

--This function returns type contract called time by combining a one contract, 
-- a scale contract and an odds contract. 
betCon :: Date -> Double -> Double -> Currency -> Contract
betCon t k o cur = time (at t) (odds (konst o) (scale (konst k) (one cur)))


bet1 = betCon (Date 5) 100 10 EUR 

