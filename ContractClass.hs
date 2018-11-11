module ContractClass where

import Contract

class (Show c) => Contracts c where

zero :: Contract
zero = Zero

time :: Obs Bool -> Date -> Contract -> Contract
time = Time

one :: Transfer -> Contract
one = One 

scale :: Double -> Contract -> Contract
scale = Scale 

get :: Contract -> Contract
get = Get

give :: Contract -> Contract
give = Give

and :: Contract -> Contract -> Contract
and = And 

oneReadable :: Contract -> Double
oneReadable (One transfer) = 1

timeReadable :: Contract -> ReadableContract
timeReadable (Time (isSettleDate)(settleDate)(Scale odds (Scale bet (One cur)))) = 
    AtContractExpire (dateReached settleDate) 
                        (expireDate settleDate) 
                            (scaleReadable (Scale odds (Scale bet (One cur))))

--scaleReadable takes a scale contract and returns Payout of type 
--ReadableContract by multiplying the odds by the bet and 
--converting to String using show
scaleReadable :: Contract -> ReadableContract
scaleReadable (Scale bet (One cur)) = 
    Payout ((currency cur) ++ show(bet * oneReadable (One cur)))
scaleReadable (Scale odds (Scale bet (One cur))) = 
    scaleReadable (Scale (odds * bet) (One cur))
