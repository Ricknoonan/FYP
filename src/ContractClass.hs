module ContractClass where

import Contract

class Contracts c where
    end :: c
    when :: Parameter -> Contract -> c
    and :: c -> c -> c
    or :: c -> c -> c
    cashIn :: InputCondition -> Contract -> c
    cashBackAll ::  Contract -> c
    send :: SendCondition -> Contract -> c
    until :: Parameter -> Contract -> c
    initiate :: Contract -> c
    function :: String -> Contract -> c 
    constructor :: Contract -> c

instance Contracts Contract where
    end = End
    when = When
    and = And
    or = Or
    cashIn = CashIn
    cashBackAll = CashBackAll
    send = Send
    until = Until
    initiate = Initiate
    function = Function 
    constructor = Constructor


