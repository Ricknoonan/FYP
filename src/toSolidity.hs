module Solidity where

import Contract
import ContractClass
import Betting
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

        (CashIn (Equal m) c1) -> [(Expr ("require(msg.value ==" ++ show (m) ++ ");"))] : sortTypes c1
        (CashIn (NoLimit) c1) -> sortTypes c1
        (CashIn (Min m) c1) -> [(Expr ("require(msg.value >" ++ show (m) ++ ");"))] : sortTypes c1
        (CashIn (Max m) c1) -> [(Expr ("require(msg.value >" ++ show (m) ++ ");"))] : sortTypes c1

        (Initiate c1) -> [(Expr "owner = msg.sender;")] : [(StVar "address owner;")] : sortTypes c1

        (Send (Owner All) c1) -> [(Expr "owner.transfer(this.balance);")] : sortTypes c1
        (Send (Winner All) c1) -> [(Expr "owner.transfer(this.balance);")] : sortTypes c1
        (Send (Person All) c1) -> [(Expr "addr.transfer(this.balance);")] : [(FunVar "address addr")] : sortTypes c1

        (Return str c1) -> [(Expr ("return " ++ str ++ ";"))] : sortTypes c1

        -- Until is checks state variables, until state variable been reached. Variables here need to be tagged as state variables
        (Until (Amount m) c1) -> [(Expr ("require (totalAmount < (this.balance + " ++ show m))] : [(StVar "uint8 totalAmount;")] : sortTypes c1 
        (Until (People p) c1) -> [(Expr ("require (peopleCount < " ++ show p ++ ");"))] : [(Expr ("peopleCount++;"))] : [(StVar ("uint8 peopleCount;"))] : sortTypes c1

        -- Days needs to be handled, figure out how end dats initialised in a context
        --(Until (Days days) c1) -> [(Expr ("require "))]
        (When (Amount m) c1) -> [(Expr ("if (totalAmount ==" ++ show m ++ "){ " ))] : sortTypes c1
        (When (People p) c1) -> [(Expr ("if (peopleCount ==" ++ show p ++ "){ " ))] : sortTypes c1

        (End) -> []

--daysToTimestamp :: Parameter -> Parameter
--daysToTimestamp d = (toInteger d) * 86400

getExpr :: [[SolTypes]] -> String
getExpr ([Expr str]:rest) = str ++ "\n" ++ getExpr rest 
getExpr ([StVar str]:rest) = getExpr rest
getExpr _ = ""

--This function will take out he "nothings" and return the actul funtion 
toString :: [[String]] -> String
toString (([str]): rest) = str ++ toString rest
toString [] = []

createFunCon :: Contract -> [[SolTypes]] -> [[String]]
createFunCon c ([Fun str] : rest) = 
    ["function " ++ str ++ (isParameter c 0 ) ++ (isPubPriv c 0)  ++ (isStateMutable c 0)++ (isReturn c 0) ++ "{" ++ getExpr rest ++ "}" ++ "\n"] : createFunCon c rest

createFunCon c ([Con] : rest) = 
    ["constructor" ++ (isParameter c 0 )  ++ (isPubPriv c 0) ++ "{" ++ getExpr rest ++ "}" ++ "\n"] : createFunCon c rest

createFunCon c ([Expr str] : rest) = 
    createFunCon c rest

createFunCon c ([StVar str] : rest) = 
    createFunCon c rest

createFunCon _ _ = []

getStVar :: [[SolTypes]] -> String
getStVar ([StVar str] : rest) = str ++ "\n" ++ getStVar rest
getStVar ([Expr str] : rest) = getStVar rest
getStVar ([Con] : rest) = getStVar rest
getStVar ([Fun str] : rest) = getStVar rest
getStVar _ = "" 
 
-- Will the function affect state? i.e.[pure|constant|view|payable]
isStateMutable :: Contract -> Int -> String 
isStateMutable (Function str c1) n = isStateMutable c1 (n+1)
isStateMutable (CashIn (inp) c1) n = "payable " ++ isStateMutable c1 n
isStateMutable _ 0 = isStateMutable c1 0
isStateMutable _ 1 = isStateMutable c1 1
isStateMutable _ 2 = "" 

--Will it have a return type? 
isReturn :: Contract -> Int -> String
isReturn (Function str c1) n = isReturn c1 (n+1)
isReturn (Return str c1) n = "pure returns(string memory) " ++ isReturn c1 n
isReturn _ 0 = isReturn c1 0
isReturn _ 1 = isReturn c1 1
isReturn _ 2 = ""

--What is types needs to have a parameter? 
isParameter :: Contract -> Int -> String 
isParameter (Function str c1) n = isParameter c1 (n+1)
isParameter _ 0 = isParameter c1 0 
isParameter _ 1 = "()"
isParameter _ 2 = ""

--What determines if a function is private? 
isPubPriv :: Contract -> Int -> String
isPubPriv (Function str c1) n = isPubPriv c1 (n+1)
isPubPriv _ 0 = isPubPriv c1 0
isPubPriv _ 1 = "public "
isPubPriv _ 2 = ""

createContract = "pragma solidity ^0.5.0;" ++ "\n" ++ "contract HelloDeposit {" ++ "\n" ++ getStVar runTypes ++ "\n" ++
                      toString(createFunCon testContract (runTypes)) ++ "\n" ++ "}"

toFile :: IO ()
toFile = writeFile "example.txt" createContract

runTypes = sortTypes testContract

test = createFunCon testContract (runTypes)

testContract :: Contract
testContract = bettingContract

c1 :: Contract
c1 = (function "deposit" (cashIn (Equal 5) End))

c2 :: Contract 
c2 = (function "sayHello" (return "HelloWorld" End))

c3 :: Contract 
c3 = (function "sayHello" (return "HelloWorld" (function "deposit" (cashIn (Equal 5) End))))

{--
--function (param types) {internal|external} [pure|constant|view|payable] [returns (return types)] varName;
--View This is generally the replacement for constant. It indicates that the function will not alter the storage state in any way.
--Pure This is even more restrictive, indicating that it won't even read the storage state.
--}