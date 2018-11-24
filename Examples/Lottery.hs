module Lottery where

import Contract
import ContractClass

data LotteryContract = LotteryContract Transfer ReadableContract ReadableContract
        deriving (Show)

lotteryContract :: Double -> Double -> Date -> Transfer -> LotteryContract
lotteryContract price sales settleDate cur = 
    LotteryContract (cur)(amount price)(time (at settleDate)(settleDate)
        (Scale (getJackpot price sales)(Scale price (One cur))))
                   

-- price of ticket. potential winnings. currency. time contract is triggered.
getJackpot :: Double -> Double -> Double
getJackpot price sales = price * sales

runlotto = lotteryContract 10 100000 (Date 10) (Currency EUR)