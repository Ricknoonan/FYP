module ContractClass where

import Contract

class Contracts c where
    zero :: c
    one :: Transfer -> c
    time :: Obs Bool -> Date -> Contract -> c
    scale :: Double -> Contract -> c
    and :: c -> c -> c

instance Contracts Contract where
    zero = Zero
    one = One
    time = Time
    scale = Scale 
    and = And 

instance Contracts ReadableContract where
    zero = Empty
    one (cur) = Single (currency cur)(oneReadable (One cur))

    time (isSettleDate)(settleDate)(Scale amountx (Scale amounty (One cur))) = 
        AtContractExpire (dateReached settleDate) (expireDate settleDate) 
            (scale amountx (Scale amounty (One cur)))
    time (isSettleDate)(settleDate)(Scale amountx (One cur)) = 
        AtContractExpire (dateReached settleDate) 
            (expireDate settleDate) (scale amountx (One cur))

    scale scalex (One cur) = Payout (currency cur) (scalex * oneReadable (One cur))
    scale scaley (Scale scalex (One cur)) = Payout (currency cur) (scalex * oneReadable (One cur) * scaley)