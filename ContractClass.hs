module ContractClass where

import Contract

class Contracts c where
    zero :: c
    time :: Date -> Contract -> Contract -> c
    scale :: Double -> Contract -> c
    and' :: c -> c -> c
    give :: c -> c
    or' :: c -> c -> c
    cashIn :: Money -> Person -> Contract -> Contract -> c
    cashOut :: Money -> Person -> Contract -> Contract -> c
    pay :: Person -> Person -> Money -> Contract -> c
    until :: Date -> Contract -> Contract -> c

instance Contracts Contract where
    zero = End
    time = Time
    scale = Scale 
    and' = And
    give = Give
    or' = Or
    cashIn = CashIn
    cashOut = CashOut
    pay = Pay
    until = Until