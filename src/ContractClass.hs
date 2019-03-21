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
    function :: String -> Contract -> c 
    constructor :: Contract -> c
    return :: String -> Contract -> c
    not :: Contract -> c
    allow :: Modifier -> Contract -> c
    set :: Parameter -> Contract -> c
    addTo :: String -> Contract -> c
    withdraw :: Contract -> c
    unless :: FunctionCondition -> Contract -> c

instance Contracts Contract where
    end = End
    when = When
    and = And
    or = Or
    cashIn = CashIn
    cashBackAll = CashBackAll
    send = Send
    until = Until
    function = Function 
    constructor = Constructor
    return = Return
    not = Not
    allow = Allow
    set = Set
    addTo = AddTo
    withdraw = Withdraw
    unless = Unless

