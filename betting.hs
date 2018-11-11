module Betting where

import Contract
import ContractClass
-- \item Amount bet {Currency}{Amount}. Odds {Odds}. Payout {Currency}{Amount*Odds} on {Date}
-- \item Amount bet €100. Odds 10/1. Payout €1000. Date 
bettingContract :: Double -> Double -> Transfer -> Date -> ReadableContract
bettingContract odds bet currency settleDate = 
    BettingContract (amountBet bet currency) (atOdds odds) (timeReadable (Time (at settleDate)(settleDate)(Scale odds (Scale bet (One currency)))))

bettingContract1 = bettingContract 10 10 (Currency USD) (Date 10) 
--BettingContract (AmountBet "USD10.0") (Odds "10.0") (AtContractExpire (DateReached False) (ExpireDate "Date 10") (Payout "USD100.0"))
