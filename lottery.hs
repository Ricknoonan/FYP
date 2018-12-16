module Lottery where

import Contract
import ContractClass

-- price of ticket. potential winnings. currency. time contract is triggered.

data LotteryContract = LotteryContract ReadableContract
        deriving (Show)

lotteryContract :: Double -> Double -> Date -> Transfer -> Contract
lotteryContract ticket sales drawDate cur = time 
                                            (at drawDate) 
                                            (scale ticket 
                                            	(scale sales 
                                            		(one cur)))

c1 :: Contract
c1 = lotteryContract 10 100000 (Date 10) (Currency EUR)

outputC :: LotteryContract
outputC = LotteryContract (evalR (c1))

-- people can continously buy tickets for the lottery until a certain time and the payout
-- is the number of tickets sold by price of ticket


-- you can commit x amount(ie buy tickets) until the time limit is true. The payout is 
--then equal to the total amount paid into the contract 