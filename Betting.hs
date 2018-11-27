module Betting where

import Contract
import ContractClass
-- \item Amount bet {Currency}{Amount}. Odds {Odds}. Payout {Currency}{Amount*Odds} on {Date}
-- \item Amount bet €100. Odds 10/1. Payout €1000. Date 

data BettingContract = BettingContract ReadableContract
         deriving (Show)

bettingContract :: Double -> Double -> Date -> Transfer -> Contract
bettingContract bet odds expireDate cur = time (at expireDate) (scale odds (scale bet (one cur)))

c1 :: Contract
c1 = bettingContract 100 (9/4) (Date 10) (Currency EUR) --put the wife and kids on it

outputC :: BettingContract
outputC = BettingContract (evalR (c1))

