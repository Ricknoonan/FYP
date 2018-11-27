module ContractClass where

import Contract

class Contracts c where
    zero :: c
    one :: Transfer -> c
    time :: Obs Bool -> Contract -> c
    scale :: Double -> Contract -> c
    and' :: c -> c -> c
    give :: c -> c
    or' :: c -> c -> c

instance Contracts Contract where
    zero = Zero
    one = One
    time = Time
    scale = Scale 
    and' = And
    give = Give
    or' = Or

{-
instance Contracts ReadableContract where
    zero = ZeroReadable "Empty"
    one (cur) = OneReadable (oneReadable (One cur))

    time (isSettleDate)(settleDate)(Scale amountx (Scale amounty (One cur))) = 
        TimeReadable (dateReached settleDate) (expireDate settleDate) 
            (scale amountx (Scale amounty (One cur)))

    time (isSettleDate)(settleDate)(Scale amount (One cur)) = 
        TimeReadable (dateReached settleDate) (expireDate settleDate) 
            (scale amount (One cur))

    scale amount (One cur) = 
        ScaleReadable (amount * oneReadable (One cur))
        
    scale scaley (Scale scalex (One cur)) = 
        ScaleReadable (scalex * oneReadable (One cur) * scaley)
    and = Join 
    -}