module ContractClass where

import Contract

end :: Contract
end = End

when :: Parameter -> Contract -> Contract
when p c = 
    case p of
        (Amount m) -> (When p c)
        (People peo) -> (When p c)
        (TotalReached ) -> (When p c)
        (TimesUp) -> (When p c)
        (TotalAmount) -> (When p c)
        _ -> (Error "Incorrect Parameter for Contract When")

and :: Contract -> Contract -> Contract
and = And

or :: Contract -> Contract -> Contract
or = Or

commitEther :: InputCondition -> Contract -> Contract
commitEther = CommitEther

cashBackAll ::  Contract -> Contract
cashBackAll = CashBackAll

send :: SendCondition -> Contract -> Contract
send = Send 

until :: Parameter -> Contract -> Contract
until p c =
    case p of 
        (Amount m) -> (Until p c)
        (TimesUp) -> (Until p c)
        (People peo) -> (Until p c)
        (TotalReached) -> (Until p c)
        _ -> (Error "Incorrect Parameter for Contract Until")

function :: String -> Contract -> Contract
function s c 
    | (s == "") = (Error "Need to give function a name")
    | otherwise = Function s c 

constructor :: Contract -> Contract
constructor = Constructor

isnot :: Contract -> Contract
isnot = IsNot

allow :: Modifier -> Contract -> Contract
allow = Allow

set :: Parameter -> Contract -> Contract
set p c = 
    case p of 
        (TotalAmount) -> Set p c
        (TimeLimit) -> Set p c
        (ContractOwner) -> Set p c
        (Beneficiary) -> Set p c
        _ -> (Error "Incorrect Parameter for Contract Set")

addTo :: String -> Contract -> Contract
addTo = AddTo

from :: String -> Contract -> Contract
from = From

withdraw :: Contract -> Contract
withdraw = Withdraw

unless :: CheckState -> Contract -> Contract
unless = Unless
