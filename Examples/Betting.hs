module Betting where

import Contract
import ContractClass
-- \item Amount bet {Currency}{Amount}. Odds {Odds}. Payout {Currency}{Amount*Odds} on {Date}
-- \item Amount bet €100. Odds 10/1. Payout €1000. Date 

data BettingContract = Odds Double |
                       BettingContract Transfer ReadableContract BettingContract ReadableContract
        deriving (Show)

oddsReadable :: Double -> BettingContract
oddsReadable x = Odds x
               
bettingContract :: Double -> Double -> Transfer -> Date -> BettingContract
bettingContract odds bet cur settleDate = 
    BettingContract (cur)
                        (amount bet) 
                            (oddsReadable odds) 
                                (time (at settleDate)
                                    (settleDate)
                                        (Scale odds(Scale bet (One cur))))
        --(time (at settleDate)(settleDate)(Scale odds (Scale bet (One currency))))

bettingContract1 = bettingContract 10 12 (Currency USD) (Date 10) 

