module ContractClass where

import Contract

class Contracts c where
    end :: c
    when :: Parameter -> Contract -> c
    scale :: Double -> Contract -> c
    and :: c -> c -> c
    give :: c -> c
    get :: c -> c
    or :: c -> c -> c
    cashIn :: InputCondition -> Contract -> c
    cashBackAll ::  Contract -> c
    send :: SendCondition -> c
    until :: Parameter -> Contract -> c
    initiate :: Contract -> c
    allow :: Parameter -> Contract -> c

instance Contracts Contract where
    end = End
    when = When
    scale = Scale 
    and = And
    give = Give
    or = Or
    cashIn = CashIn
    cashInUnlimited = CashInUnlimited
    cashBackAll = CashBackAll
    send = Send
    until = Until
    initiate = Initiate
    get = Get
    allow = Allow