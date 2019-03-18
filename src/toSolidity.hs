module Solidity where

import Contract
import ContractClass
import Betting
import CrowdFunding
import Prelude hiding (until, interact,return)

data SolTypes =   FunVar String |
                  StVar String |
                  Expr String |
                  Fun String |
                  Con |
                  SolTypes 
                deriving (Show)

data StateTypes = OwnerAddress String String|
                  OtherAddress String |
                  Mapping String String |
                  Time String String|
                  Unit String String
                deriving (Show)

-- Trying to sort contracts into SolTypes and put in array. 

sortStateTypes :: Contract -> [[SolTypes]]
sortStateTypes c = 
	case c of = 
        (CashIn (Higher str1) (AddTo str2 c1)) -> [(OtherAddress ("address public") (str1))] : 
                                                  [(Mapping ("mapping(address => uint256) public" ++ str2 ++ ";"))] 
        (Initiate c1) -> [(OwnerAddress ("address") ("owner")] : sortTypes c1
        (Until (Amount m) c1) -> [(Unit ("uint8 ") ("totalAmount"))] : sortTypes c1 
        (Until (People p) c1) ->  [(Unit ("uint8") ("peopleCount"))] : sortTypes c1
        (Until (TimesUp) c1) -> [(Time ("uint public")("end"))]
        (End) -> []

getStateType :: String -> [[SolTypes]] -> String
getStateType "time" s = 
	case s of 
		(Time str1 str2) -> str2
		_ -> getStateType "time" s

getStateType "ota" s = 
	case s of 
		(OtherAddress str1 str2) -> str2
		_ -> getStateType "ota" s

getStateType "map" s = 
	case s of 
		(Mapping str1 str2) -> str2
		_ -> getStateType "map" s

getStateType "unit" s = 
	case s of 
		(Unit str1 str2) -> str2
		_ -> getStateType "unit" s

getStateType "oa" s = 
	case s of 
		(OwnerAddress str1 str2) -> str2
		_ -> getStateType "oa" s

sortTypes :: Contract -> [[SolTypes]]
sortTypes c = 
    case c of 
        (Constructor c1) -> [(Con )] : sortTypes c1
        (Function str c1) -> [(Fun (str))] : sortTypes c1

        (CashIn (Equal m) c1) -> [(Expr ("require(msg.value ==" ++ show (m) ++ ");"))] : sortTypes c1
        (CashIn (NoLimit) c1) -> sortTypes c1
        (CashIn (Min m) c1) -> [(Expr ("require(msg.value >" ++ show (m) ++ ");"))] : sortTypes c1
        (CashIn (Max m) c1) -> [(Expr ("require(msg.value >" ++ show (m) ++ ");"))] : sortTypes c1
        (CashIn (Higher str1) (AddTo str2 c1)) ->  [(Expr ("unit new" ++ str2 ++ "= (msg.value +" ++ str2 ++ "[msg.sender]"))] : 
                                                   [(Expr ("require ((msg.value +" ++ str2 ++ "[msg.sender]) + msg.value >" ++ str1 ++ ";"))] :
                                                   [(Expr (str2 ++ "= new" ++ str1))] : sortTypes c1

        (Initiate c1) -> [(Expr "owner = msg.sender;")] : [(StVar "address owner;")] : sortTypes c1

        (Send (Owner All) c1) -> [(Expr "owner.transfer(this.balance);")] : sortTypes c1
        (Send (Winner All) c1) -> [(Expr "owner.transfer(this.balance);")] : sortTypes c1
        (Send (Person All) c1) -> [(Expr "addr.transfer(this.balance);")] : 
                                  [(FunVar "address addr")] : sortTypes c1

        (Return str c1) -> [(Expr ("return " ++ str ++ ";"))] : sortTypes c1

        -- Until is checks state variables, until state variable been reached. Variables here need to be tagged as state variables
        (Until (Amount m) c1) -> [(Expr ("require (totalAmount < (this.balance + " ++ show m))] : 
        (Until (People p) c1) -> [(Expr ("require (peopleCount < " ++ show p ++ ");"))] : 
                                 [(Expr ("peopleCount++;"))] : sortTypes c1
        (Until (TimesUp) c1) ->  [(Expr ("require (now < " ++ (getStateType "time" (sortStateTypes c)) )]
        -- Days needs to be handled, figure out how end dats initialised in a context
        --(Until (Days days) c1) -> [(Expr ("require "))]

        --Needs to be updated, need to identify when if statement is finished
        (When (Amount m) c1) -> [(Expr ("if (totalAmount ==" ++ show m ++ "){ " ))] : sortTypes c1
        (When (People p) c1) -> [(Expr ("if (peopleCount ==" ++ show p ++ "){ " ))] : sortTypes c1

        (End) -> []


-- Soltypes sorted and then sorted into control flow. Determine whats in an if statement, whats in a while..

--daysToTimestamp :: Parameter -> Parameter
--daysToTimestamp d = (toInteger d) * 86400

getExpr :: [[SolTypes]] -> String
getExpr ([Expr str]:rest) = str ++ "\n" ++ getExpr rest 
getExpr ([StVar str]:rest) = getExpr rest
getExpr _ = ""

getStVar :: [[SolTypes]] -> String
getStVar ([StVar str] : rest) = str ++ "\n" ++ getStVar rest
getStVar ([Expr str] : rest) = getStVar rest
getStVar ([Con] : rest) = getStVar rest
getStVar ([Fun str] : rest) = getStVar rest
getStVar ([FunVar str] : rest) = getStVar rest
getStVar _ = "" 

getParameters :: [[SolTypes]] -> Int -> String
getParameters ([FunVar str] : rest) n 
    |(n == 0) = str ++ getParameters rest (n+1)
    |(n > 0 ) = "," ++ str ++ getParameters rest n
getParameters ([Expr str] : rest) n= getParameters rest n
getParameters ([StVar str] : rest) n = getParameters rest n
getParameters ([Fun str] : rest) n = ""
getParameters ([Con] : rest) n = ""
getParameters _ n = ""

--This function will take out he "nothings" and return the actul funtion 
toString :: [[String]] -> String
toString (([str]): rest) = str ++ toString rest
toString [] = []

createFunCon :: Contract -> [[SolTypes]] -> [[String]]
createFunCon c ([Fun str] : rest) = 
    ["function " ++ str ++ (isParameter c 0 rest) ++ (isPubPriv c 0)  ++ (isStateMutable c 0)++ (isReturn c 0) ++ "{" ++ getExpr rest ++ "}" ++ "\n"] : createFunCon c rest

createFunCon c ([Con] : rest) = 
    ["constructor" ++ (isParameter c 0 rest)  ++ (isPubPriv c 0) ++ "{" ++ getExpr rest ++ "}" ++ "\n"] : createFunCon c rest

createFunCon c ([Expr str] : rest) = 
    createFunCon c rest

createFunCon c ([StVar str] : rest) = 
    createFunCon c rest

createFunCon c ([FunVar str] : rest) = 
    createFunCon c rest

createFunCon _ _ = []
 
-- Will the function affect state? i.e.[pure|constant|view|payable]
isStateMutable :: Contract -> Int -> String 
isStateMutable (Function str c1) n = isStateMutable c1 (n+1)
isStateMutable (CashIn (inp) c1) 1 = "payable " ++ isStateMutable c1 1
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
isParameter :: Contract -> Int -> [[SolTypes]] -> String 
isParameter (Function str c1) n s = isParameter c1 (n+1) s
isParameter (Constructor c1) n s = isParameter c1 (n+1) s
isParameter _ 0 s = isParameter c1 0 s
isParameter _ 1 s = "(" ++ getParameters s 0 ++ ")"
isParameter _ 2 s = ""

--What determines if a function is private? 
isPubPriv :: Contract -> Int -> String
isPubPriv (Function str c1) n = isPubPriv c1 (n+1)
isPubPriv _ 0 = isPubPriv c1 0
isPubPriv _ 1 = "public "
isPubPriv _ 2 = ""

--isMapNeeded 

createContract = "pragma solidity ^0.5.0;" ++ "\n" ++ "contract HelloDeposit {" ++ "\n" ++ getStVar runTypes ++ "\n" ++
                      toString(createFunCon testContract (runTypes)) ++ "\n" ++ "}"

toFile :: IO ()
toFile = writeFile "example.txt" createContract

runTypes = sortTypes testContract

test = createFunCon testContract (runTypes)

testContract :: Contract
testContract = crowdFunding

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