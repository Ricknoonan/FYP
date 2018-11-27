module Futures where

import ContractClass
import Contract

data FuturesContract = FuturesContract ReadableContract
        deriving (Show)
--zero coubon bond example used in paper
zcb :: Date -> Double -> Transfer -> Contract
zcb t x k = time (at t) (scale (x) (one k))

futures :: Date -> Double -> Transfer -> Contract
futures date price cur = give (zcb date price cur)

sendMoney :: Contract
sendMoney = futures (Date 10) 100 (Currency EUR)

outputC :: FuturesContract
outputC = FuturesContract (evalR (sendMoney))