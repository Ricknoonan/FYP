module ContractClass where

import Contract

class Contracts c where
    zero :: c
    when :: ControlObs -> Contract -> Contract -> c
    scale :: Double -> Contract -> c
    and' :: c -> c -> c
    give :: c -> c
    or' :: c -> c -> c
    cashAndPeople :: Money -> Person  -> Contract -> Contract -> c
    cashIn :: Money -> Address -> Contract -> Contract -> Contract -> c
    cashInUnlimited :: Address -> Contract -> Contract -> Contract -> c
    cashOut ::  Contract -> Contract -> c
    pay :: ControlObs -> Contract -> c
    until :: ControlObs -> Contract -> Contract -> c
    people :: Int -> c

instance Contracts Contract where
    zero = End
    when = When
    scale = Scale 
    and' = And
    give = Give
    or' = Or
    cashIn = CashIn
    people = People
    cashInUnlimited = CashInUnlimited
    cashOut = CashOut
    pay = Pay
    until = Until