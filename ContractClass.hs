module ContractClass where

import Contract

class Contracts c where
    zero :: c
    time :: Obs Bool -> Date -> c -> c
    one :: Transfer -> c
    scale :: Double -> c -> c
    timeReadable :: c -> ReadableContract
    scaleReadable :: c -> ReadableContract

instance Contracts Contract where
    zero = Zero
    time = Time
    one = One
    scale = Scale 

    timeReadable (Time (isSettleDate)(settleDate)(Scale odds (Scale bet (One cur)))) = 
        AtContractExpire (dateReached settleDate) 
                (expireDate settleDate) 
                        (scaleReadable (Scale odds (Scale bet (One cur))))
    --scaleReadable takes a scale contract and returns Payout of type 
    --ReadableContract by multiplying the odds by the bet and 
    --converting to String using show
    scaleReadable (Scale bet (One cur)) = 
        Payout ((currency cur) ++ show(bet * oneReadable (One cur)))
    scaleReadable (Scale odds (Scale bet (One cur))) = 
        scaleReadable (Scale (odds * bet) (One cur))    
    




