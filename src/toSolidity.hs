module Solidity where

import Contract
import ContractClass
import Prelude hiding (until, interact,return)

data SolTypes =   FunVar String |
                  StVar String |
                  Expr String |
                  Fun String |
                  Con |
                  SolTypes 
                deriving (Show)

-- Trying to sort contracts into SolTypes and put in array. 
sortTypes :: Contract -> [[SolTypes]]
sortTypes c = 
    case c of 
        (Constructor c1) -> [(Con )] : sortTypes c1
        (Function str c1) -> [(Fun (str))] : sortTypes c1
        (CashIn (Equal m) c1) ->  [(Expr ("require(msg.value ==" ++ show (m) ++ ");"))] : sortTypes c1
        (CashIn (NoLimit) c1) -> sortTypes c1
        (Initiate c1) -> [(Expr "owner = msg.sender;")] : sortTypes c1
        (Send (Owner All) c1) -> [(Expr "owner.transfer(this.balance);")] : sortTypes c1
        (Return str c1) -> [(Expr ("return " ++ str ++ ";"))] : sortTypes c1
        (End) -> []

-- get string out of expression, only works when there is one expression like eg below
getExpr :: [[SolTypes]] -> String
getExpr ([Expr str]:rest) = str 

--This function will take out he "nothings" and return the actul funtion 
toString :: [[String]] -> String
toString (([str]): rest) = str ++ toString rest
toString [] = []

createFunctions :: Contract -> [[SolTypes]] -> [[String]]
createFunctions c ([Fun str] : rest) = 
    ["function " ++ str ++ (isParameter c 0) ++ (isPubPriv c 0)  ++ (isStateMutable c 0)++ (isReturn c 0) ++ "{" ++ getExpr rest ++ "}"] : createFunctions c rest

createFunctions c ([Con] : rest) = 
    ["constructor"] : createFunctions c rest

createFunctions c ([Expr str] : rest) = 
    createFunctions c rest

createFunctions _ _ = []

isStateMutable :: Contract -> Int -> String 
isStateMutable (Function str c1) n = isStateMutable c1 (n+1)
isStateMutable (CashIn (inp) c1) n = "payable " ++ isStateMutable c1 n
isStateMutable _ 1 = isStateMutable c1 1
isStateMutable _ 2 = "" 

isReturn :: Contract -> Int -> String
isReturn (Function str c1) n = isReturn c1 (n+1)
isReturn (Return str c1) n = "pure returns(string memory) " ++ isReturn c1 n
isReturn _ 1 = isReturn c1 1
isReturn _ 2 = ""

isParameter :: Contract -> Int -> String 
isParameter (Function str c1) n = isParameter c1 (n+1)
isParameter _ 1 = "() " ++ isParameter c1 1
isParameter _ 2 = ""

isPubPriv :: Contract -> Int -> String
isPubPriv (Function str c1) n = isPubPriv c1 (n+1)
isPubPriv _ 1 = "public "
isPubPriv _ 2 = ""

testContract :: Contract
testContract = c3

c1 :: Contract
c1 = (function "deposit" (cashIn (Equal 5) End))

c2 :: Contract 
c2 = (function "sayHello" (return "HelloWorld" End))

c3 :: Contract 
c3 = (function "sayHello" (return "HelloWorld" (function "deposit" (cashIn (Equal 5) End))))


createContract = "pragma solidity ^0.5.0;" ++ "\n" ++ "contract HelloDeposit {" ++ "\n" ++
                      toString(createFunctions testContract (runTypes)) ++ "\n" ++ "}"

toFile :: IO ()
toFile = writeFile "example.txt" createContract

runTypes = sortTypes testContract

test = createFunctions testContract (runTypes)

{--
--function (param types) {internal|external} [pure|constant|view|payable] [returns (return types)] varName;
--View This is generally the replacement for constant. It indicates that the function will not alter the storage state in any way.
--Pure This is even more restrictive, indicating that it won't even read the storage state.
--}