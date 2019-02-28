module Solidity where

import Contract
import ContractClass
import Prelude hiding (until, interact)

data SolTypes =   FunVar String |
                  StVar String |
                  Expr String |
                  Fun String |
                  Con String |
                  SolTypes 
                deriving (Show)

-- Trying to sort contracts into SolTypes and put in array. 
sortTypes :: Contract -> [[SolTypes]]
sortTypes c = 
    case c of 
        (Constructor c1) -> [(Con "constructor")] : sortTypes c1
        (Function str c1) -> [(Fun (str))] : sortTypes c1
        (CashIn (Equal m) c1) ->  [(Expr ("require(msg.value ==" ++ show (m) ++ ");"))] : sortTypes c1
        (CashIn (NoLimit) c1) -> sortTypes c1
        (Initiate c1) -> [(Expr "owner = msg.sender;")] : sortTypes c1
        (Send (Owner All) c1) -> [(Expr "owner.transfer(this.balance);")] : sortTypes c1
        (End) -> []

-- Creates function outline and puts expressions inbetween brackets, pretty verbose for now, needs changing
createFunctions :: Contract -> [[SolTypes]] -> String
createFunctions c ([Fun str]:rest) 
    | (isPayPub c) = "function " ++ str ++ "()" ++ " public payable" ++ "{" ++ getExpr rest ++ "}"
    | otherwise = "function " ++ str ++ "()" ++ "public" ++ "{" ++ getExpr rest ++ "}"

-- get string out of expression, only works when there is on expression like eg below
getExpr :: [[SolTypes]] -> String
getExpr ([Expr str]:rest) = str 

-- This checks if the function is a payable function i.e requires you to commit ether, identified by the CashIn contract
-- Payable and public are lumped into this function for now but will need two sperate functions to determine payable and/or public
isPayPub :: Contract -> Bool 
isPayPub (Function str c1) = isPayPub c1
isPayPub (CashIn (inp) c1) = True 
isPayPub _ = False 

{--
checkNext :: Contract -> Bool
checkNext c = 
    case c of 
        (Constructor c1) -> True
        (Function str c1) -> True
        _ -> False
--}

testContract :: Contract
testContract = (function "deposit" (cashIn (Equal 5) End))

runTypes = sortTypes testContract

testCreate = createFunctions testContract (runTypes)

--[[Con, Expression "some exp", Expression, Expression],[Function, Expression,Expression]]