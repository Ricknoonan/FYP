module ContractClass where

import Contract

class Contracts c where
    zero :: c
    time :: Obs Bool -> Date -> Contract -> c
    one :: Transfer -> c
    scale :: Double -> Contract -> c

instance Contracts Contract where
    zero = Zero
    time = Time
    one = One
    scale = Scale  

instance Contracts ReadableContract where
    zero = Empty

    time (isSettleDate)(settleDate)(Scale odds (Scale bet (One cur))) =
        AtContractExpire (dateReached settleDate) 
                (expireDate settleDate) 
                        (scale odds (Scale bet (One cur)))

    scale odds (Scale bet (One cur)) = 
        Payout ((currency cur) ++ show(odds * bet * oneReadable (One cur)))