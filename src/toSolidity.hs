module ToSolidity where

import Contract
import ContractClass
import Bank 
import Lottery 
import Auction 
import Prelude hiding (until, interact,return)

data SolTypes =   FunVar String |
                  StVar String |
                  ReturnVar String |
                  Expr String |
                  PayExpr String |
                  IfExpr String |
                  ReturnExpr String|
                  Fun String |
                  RequireExpr String |
                  Con 
                deriving (Show)

data StateTypes = OwnerAddress String String|
                  OtherAddress String String |
                  Mapping String String |
                  Time String String|
                  Unit String String |
                  List String String |
                  Count String String |
                  Bool String String |
                  HighestAddress String String |
                  HighestUnit String String
                deriving (Show)

-- Takes the contract and sorts into StateTypes with two strins; first being data type and the second being variable name. This is done so 
-- the state variables can be used in functions 
sortStateTypes :: Contract -> [[StateTypes]]
sortStateTypes c = 
    case c of 
        (Set (ContractOwner) c1) -> [(OwnerAddress ("address") ("owner"))] : sortStateTypes c1
        (Set (TimeLimit) c1) -> sortStateTypes c1
        (Set (Beneficiary ) c1) -> [OtherAddress ("address payable public") ("beneficiary")] : sortStateTypes c1

        (Until (Amount m) c1) -> [(Unit ("uint8 ") ("totalAmount"))] : sortStateTypes c1
        (Until (People p) c1) ->  case c1 of
                                    (Unless (AlreadyJoined) c2) -> [(List ("address payable " ++ "[" ++ show p ++ "]") ("participants"))] : [(Count ("uint") ("peopleCount"))] : sortStateTypes c2
                                    (_) ->  [(Count ("uint") ("peopleCount"))] : sortStateTypes c1
        (Until (TimesUp) c1) -> [(Time ("uint public")("end"))] : sortStateTypes c1

        (Withdraw c1) -> sortStateTypes c1

        (Function str c1) -> sortStateTypes c1

        (Send (Random All) c1 ) -> [(Unit ("uint") ("randNonce"))] : sortStateTypes c1
        (Send (Winner All) c1) -> sortStateTypes c1
        (Send (Winner ((AmountIn str))) c1) -> sortStateTypes c1
        (Send (ToOwner All) c1) -> sortStateTypes c1 
        (Send (ToBeneficiary (AmountIn str)) c1) -> sortStateTypes c1
        (Send (Address All) c1) -> sortStateTypes c1

        (CommitEther (NoLimit) c1) -> case c1 of 
                                   (AddTo str c2) -> [(Mapping ("mapping (address => uint) private") (str))] : sortStateTypes c2
                                   (_) -> sortStateTypes c1 
        (CommitEther (HigherThan str1) c1) -> case c1 of 
                                        (AddTo str2 c2) -> [(HighestAddress ("address public") (str1 ++ "Address"))] : 
                                                           [(Mapping ("mapping(address => uint) public") (str2))] : 
                                                           [(HighestUnit ("uint public") (str2 ++ "Highest"))] : sortStateTypes c2                                                
                                        (_) -> sortStateTypes c1
        (CommitEther (Equal m) c1) -> case c1 of 
                                      (AddTo str c2) -> [(Mapping ("mapping (address => uint) private") (str))] : sortStateTypes c2
                                      (_) -> sortStateTypes c1

        (Unless (AlreadyJoined ) c1) -> sortStateTypes c1

        (Unless (AlreadyFinished) c1) -> [(Bool ("bool") ("ended"))] : sortStateTypes c1

        (Constructor c1) -> sortStateTypes c1

        (Allow (NotOwner) c1) -> sortStateTypes c1
        (Allow (OnlyOwner) c1) -> sortStateTypes c1

        (When (TimesUp) c1) -> sortStateTypes c1
        (When (People p) c1) -> sortStateTypes c1

        (From str c1) -> sortStateTypes c1

        (End) -> []

--Takes the identifer string and the sorted state types and returns the variable name to be used in the function bodies
getStateType :: String -> [[StateTypes]] -> String
getStateType "time" s = 
    case s of 
        ([Time str1 str2] : rest) -> str2 ++ ");"
        ([_] : rest) -> getStateType "time" rest
getStateType "ota" s = 
    case s of 
        ([OtherAddress str1 str2] : rest) -> str2
        ([_] : rest) -> getStateType "ota" rest
getStateType "map" s = 
    case s of 
        ([Mapping str1 str2] : rest) -> str2
        ([_] : rest) -> getStateType "map" rest
getStateType "unit" s = 
    case s of 
        ([Unit str1 str2]: rest) -> str2
        ([_] : rest) -> getStateType "unit" rest
getStateType "oa" s = 
    case s of 
        ([OwnerAddress str1 str2] : rest) -> str2
        ([_] : rest) -> getStateType "oa" rest
getStateType "high" s = 
    case s of 
        ([HighestAddress str1 str2] : rest) -> str2
        ([_] : rest) -> getStateType "high" rest

getStateType "highUint" s =
  case s of 
        ([HighestUnit str1 str2] : rest) -> str2
        ([_] : rest) -> getStateType "highUint" rest


-- Helper function that takes the contract, sortTypes takes the contract and the sorted state types. This is done so the state types only needed to be sorted once 
-- and avoid duplication 
stateTypesToSort :: Contract -> [[SolTypes]]
stateTypesToSort c = sortTypes c (sortStateTypes c)


--Takes the contract and the sorted statetypes and returns the soltypes with the state variable names included 
sortTypes :: Contract -> [[StateTypes]] -> [[SolTypes]]
sortTypes c s = 
    case c of 
        (Constructor c1) ->  [(Con )] : sortTypes c1 s
        (Function str c1) -> [(Fun (str))] : sortTypes c1 s


        (CommitEther (Equal m) c1) -> [(PayExpr (commitEtherSolidity "==" m))] : sortTypes c1 s
        (CommitEther (Min m) c1) ->   [(PayExpr (commitEtherSolidity ">" m))] : sortTypes c1 s
        (CommitEther (Max m) c1) ->   [(PayExpr (commitEtherSolidity "<" m))] : sortTypes c1 s
        (CommitEther (NoLimit) c1) -> case c1 of  
                                    (AddTo str c2) -> [(PayExpr ((getStateType "map" s) ++ "[msg.sender] += msg.value;"))] :
                                                      [(ReturnExpr ( "return " ++ (getStateType "map" s) ++ "[msg.sender];"))] :
                                                      [(ReturnVar ("uint"))] : sortTypes c2 s
                                    (_) -> sortTypes c1 s       
        (CommitEther (HigherThan str1) c1) -> case c1 of 
                                        (AddTo str2 c2) -> [(PayExpr ("require(msg.value > " ++ str2 ++ "Highest);"))] :
                                                           [(IfExpr ("if (" ++ str2 ++ "Highest != 0)" ))] :
                                                           [(Expr ((getStateType "map" s)++"[" ++ (getStateType "high" s) ++ "] += " ++ str2 ++ "Highest; }"))] :
                                                           [(Expr ((getStateType "high" s) ++ "= msg.sender;"))] : 
                                                           [(Expr ((str2 ++ "Highest") ++ "= msg.value;"))] : sortTypes c2 s                                                  
                                        (_) -> sortTypes c1 s

        (Until (People p) c1) -> case c1 of
                                    (Unless (AlreadyJoined) c2) -> [(Expr ("require (peopleCount < " ++ show p ++ ");"))] :
                                                                [(Expr ("require (!joinedAlready(msg.sender));"))] :
                                                                [(Expr ("participants[peopleCount] = msg.sender;"))] :
                                                                [(Expr ("peopleCount++;"))] : sortTypes c2 s
                                    (_) ->                      [(Expr ("require ( peopleCount < " ++ show p ++ ");"))] : 
                                                                [(Expr ("peopleCount++;"))] : sortTypes c1 s
        (Until (Amount m) c1) -> [(Expr ("require (totalAmount < (this.balance + " ++ show m))] : sortTypes c1 s
        (Until (TimesUp) c1) -> [(Expr ("require (now <= " ++ (getStateType "time" s)))] : sortTypes c1 s

         

        (Set (ContractOwner)c1) -> [(Expr "owner = msg.sender;")] : sortTypes c1 s
        (Set (TimeLimit) c1) -> [(Expr "end = now + time;")] : 
                                [(FunVar "uint time")] : sortTypes c1 s
        (Set (Beneficiary) c1) -> [(Expr "beneficiary = _beneficiary;")] : 
                                  [(FunVar "address payable _beneficiary")] : sortTypes c1 s
        (Send (ToOwner All) c1) -> [(Expr sendAllSolidity)] : sortTypes c1 s
        (Send (Winner All) c1) ->  [(Expr sendAllSolidity)] : sortTypes c1 s
        (Send (Address All) c1) -> case c1 of 
                                       (From str c2) -> [(Expr ("uint amount =" ++ str ++ "[msg.sender];"))] : 
                                                        [(IfExpr ("if (amount > 0)"))] : 
                                                        [(Expr (str ++ "[msg.sender] = 0;"))] : 
                                                        [(Expr ("msg.sender.transfer(amount);"))] : sortTypes c1 s
                                       (_) -> [(Expr "addr.transfer(this.balance);")] : 
                                              [(FunVar "address addr")] : sortTypes c1 s
        (Send (Random All) c1) ->  [(Expr "address payable winner = participants[randomNumber()];")] : 
                                   [(Expr "winner.transfer(address(this).balance);")] : 
                                   [(ReturnExpr "return winner;")] : 
                                   [(ReturnVar "address")] : sortTypes c1 s

        (Send (ToBeneficiary (AmountIn str)) c1) -> [(PayExpr ("beneficiary.transfer(" ++ (getStateType "highUint" s) ++");"))] : sortTypes c1 s

        (Withdraw c1) -> [(IfExpr ("if (withdrawAmount <=" ++ (getStateType "map" s) ++ "[msg.sender]) "))] :
                         [(Expr  ((getStateType "map" s) ++ "[msg.sender] -= withdrawAmount;"))] :
                         [(Expr ("msg.sender.transfer(withdrawAmount);}"))] :
                         [(ReturnExpr ("return " ++ (getStateType "map" s) ++"[msg.sender];"))] : 
                         [(FunVar ("uint withdrawAmount"))] : 
                         [(ReturnVar ("uint"))] : sortTypes c1 s
        

        (When (Amount m) c1) -> [(IfExpr ("if (totalAmount ==" ++ show m ++ ") " ))] : sortTypes c1 s
        (When (People p) c1) -> [(IfExpr ("if (peopleCount ==" ++ show p ++ ") " ))] : sortTypes c1 s
        (When (TimesUp) c1) -> sortTypes c1 s

        (Unless (AlreadyJoined) c1) -> [(Expr ("require (!joinedAlready(msg.sender));"))] : sortTypes c1 s
        (Unless (AlreadyFinished) c1) -> [(Expr ("require (!ended);"))] :
                                         [(Expr ("ended = true;"))] : sortTypes c1 s

        (Allow (NotOwner) c1) -> [(Expr ("require (msg.sender) != owner;"))] : sortTypes c1 s
        (Allow (OnlyOwner) c1) -> [(Expr ("require (msg.sender) == owner;"))] : sortTypes c1 s
        (From str c1) -> sortTypes c1 s
        (End) -> []

-- Soltypes sorted and then sorted into control flow. Determine whats in an if statement, whats in a while..

--daysToTimestamp :: Parameter -> Parameter
--daysToTimestamp d = (toInteger d) * 86400

--Helper function that forms a a solidity expression 
combineString :: String -> String -> String
combineString str1 str2 = str1 ++ " " ++ str2 ++ ";" ++ "\n"

ifBody :: [[SolTypes]] -> String 
ifBody ([PayExpr str]:rest) = str ++ "\n" ++ ifBody rest
ifBody ([Expr str] : rest) = str ++ "\n" ++ ifBody rest
ifBody ([StVar str] : rest) = ifBody rest
ifBody ([FunVar str ] : rest) = ifBody rest
ifBody ([ReturnVar str] : rest) = ifBody rest
ifBody ([ReturnExpr str] : rest) = str ++ "\n" ++ "}" ++ createBody rest
ifBody _ = ""
 
-- Unwraps Expressions, stops unwrapping at fun or con denoted as _ 
createBody :: [[SolTypes]] -> String
createBody ([IfExpr str] : rest) = str ++ "{" ++ "\n" ++ ifBody rest 
createBody ([Expr str]:rest) = str ++ "\n" ++ createBody rest 
createBody ([PayExpr str]:rest) = str ++ "\n" ++ createBody rest 
createBody ([ReturnExpr str] : rest) = str ++ "\n" ++ createBody rest 
createBody ([StVar str]:rest) = createBody rest
createBody ([FunVar str]:rest) = createBody rest
createBody ([ReturnVar str]:rest) = createBody rest
createBody _ = "\n"

-- Recombines seperated state variables so that they can be printed correctly 
getStVar :: [[StateTypes]] -> String
getStVar ([OwnerAddress str1 str2] : rest) = combineString str1 str2 ++ getStVar rest
getStVar ([OtherAddress str1 str2] : rest) = combineString str1 str2 ++ getStVar rest
getStVar ([Mapping str1 str2] : rest) = combineString str1 str2 ++ getStVar rest
getStVar ([Time str1 str2] : rest) = combineString str1 str2 ++ getStVar rest
getStVar ([Unit str1 str2] : rest) = combineString str1 str2 ++ getStVar rest
getStVar ([Count str1 str2] : rest) = combineString str1 str2 ++ getStVar rest
getStVar ([List str1 str2] : rest) = combineString str1 str2 ++ getStVar rest
getStVar ([Bool str1 str2] : rest) = combineString str1 str2 ++ getStVar rest
getStVar ([HighestAddress str1 str2] : rest) = combineString str1 str2 ++ getStVar rest
getStVar ([HighestUnit str1 str2] : rest) = combineString str1 str2 ++ getStVar rest
getStVar _ = "\n" 

--This function will take out he "nothings" and return the actul funtion 
toString :: [[String]] -> String
toString (([str]): rest) = str ++ toString rest
toString [] = []

-- Takes the contract and the sorted Soltypes and forms function/constructor outline and gets the expressions for that fun/con. 
-- If function or constructor matches is defines the makeup up of that fun/con and pass the rest of the types to getExpr which finds the expressions within that function.
-- Once the exprs withing that function have been found, the rest loops back until a function or contstuctor is found again and repeats
createFunCon :: Contract -> [[SolTypes]] -> [[String]]
createFunCon c ([Fun str] : rest) = 
    ["function " ++ str ++ (isParameter rest) ++ (isPubPriv c 0)  ++ (isStateMutable rest)++ (getReturns rest) ++ "{" ++ "\n" ++ createBody rest ++ "}" ++ "\n"] : createFunCon c rest
createFunCon c ([Con] : rest) = 
    ["constructor" ++ (isParameter rest)  ++ (isPubPriv c 0) ++ "{" ++ "\n" ++ createBody rest ++ "}" ++ "\n"] : createFunCon c rest
createFunCon c ([Expr str] : rest) = createFunCon c rest
createFunCon c ([PayExpr str] : rest) = createFunCon c rest
createFunCon c ([StVar str] : rest) = createFunCon c rest
createFunCon c ([FunVar str] : rest) = createFunCon c rest
createFunCon c ([ReturnVar str] : rest) = createFunCon c rest
createFunCon c ([IfExpr str] : rest) = createFunCon c rest
createFunCon c ([ReturnExpr str] : rest) = createFunCon c rest 
createFunCon _ _ = []

createStandardFun :: [[SolTypes]] -> [[String]]
createStandardFun ([(Expr ("require (!joinedAlready(msg.sender));"))] : rest) =  ["function joinedAlready(address payable participant) private view returns(bool) {"
                                                  ++ "\n" ++ "for(uint i = 0; i < participants.length; i++) {"
                                                  ++ "\n" ++ "if (participants[i] == participant) {"
                                                  ++ "\n" ++ "return true;"
                                                  ++ "\n" ++ "}" ++ "\n" ++ "}"
                                                  ++ "\n" ++ "return false;" ++ "\n" ++ "}"] : createStandardFun rest

createStandardFun ([(Expr ("address payable winner = participants[randomNumber()];"))] : rest) = ["function randomNumber() private returns(uint) {"
                                          ++ "\n" ++ "uint rand = uint(keccak256(abi.encodePacked(now, msg.sender, randNonce))) % 10;"
                                          ++ "\n" ++ "randNonce++;"
                                          ++ "\n" ++ "return rand;}"] : createStandardFun rest

createStandardFun ([Expr str] : rest) = createStandardFun rest
createStandardFun ([PayExpr str] : rest) = createStandardFun rest
createStandardFun ([StVar str] : rest) = createStandardFun rest
createStandardFun ([FunVar str] : rest) = createStandardFun rest
createStandardFun ([ReturnVar str] : rest) = createStandardFun rest
createStandardFun ([IfExpr str] : rest) = createStandardFun rest
createStandardFun ([ReturnExpr str] : rest) = createStandardFun rest 
createStandardFun ([Fun str] : rest) = createStandardFun rest
createStandardFun ([Con] : rest) = createStandardFun rest
createStandardFun _ = []

                                            
 -- Will the function affect state? i.e.[pure|constant|view|payable]
isStateMutable :: [[SolTypes]] -> String 
isStateMutable ([PayExpr str] : rest) = "payable "
isStateMutable ([ReturnVar str] : rest) = isStateMutable rest
isStateMutable ([Expr str] : rest) = isStateMutable rest 
isStateMutable ([StVar str] : rest) = isStateMutable rest 
isStateMutable ([FunVar str] : rest) = isStateMutable rest 
isStateMutable ([IfExpr str] : rest) = isStateMutable rest 
isStateMutable ([ReturnExpr str] : rest) = isStateMutable rest
isStateMutable ([RequireExpr str] : rest) = isStateMutable rest 
isStateMutable ([Fun str] : rest) = ""
isStateMutable ([Con] : rest) = ""
isStateMutable _ = ""

getReturns :: [[SolTypes]] -> String 
getReturns ([ReturnVar str] : rest) = "returns (" ++ str ++ ")"
getReturns ([ReturnExpr str] : rest) = getReturns rest
getReturns ([Expr str] : rest) = getReturns rest 
getReturns ([PayExpr str] : rest) = getReturns rest 
getReturns ([IfExpr str] : rest) = getReturns rest
getReturns ([StVar str] : rest) = getReturns rest 
getReturns ([FunVar str] : rest) = getReturns rest
getReturns ([RequireExpr str] : rest) = getReturns rest 
getReturns ([Fun str] : rest) = ""
getReturns ([Con] : rest) = ""
getReturns _ = ""


--What is types needs to have a parameter? 
isParameter :: [[SolTypes]] -> String 
isParameter s = "(" ++ getParameters s 0 ++ ")"

getParameters :: [[SolTypes]] -> Int -> String
getParameters ([FunVar str] : rest) n 
    |(n == 0) = str ++ getParameters rest (n+1)
    |(n > 0 ) = "," ++ str ++ getParameters rest n
getParameters ([Expr str] : rest) n= getParameters rest n
getParameters ([PayExpr str] : rest) n= getParameters rest n
getParameters ([StVar str] : rest) n = getParameters rest n
getParameters ([Fun str] : rest) n = ""
getParameters ([Con] : rest) n = ""
getParameters ([ReturnVar str] : rest) n = getParameters rest n
getParameters ([IfExpr str] : rest) n = getParameters rest n
getParameters ([RequireExpr str] : rest) n = getParameters rest n
getParameters ([ReturnExpr str] : rest) n = getParameters rest n
getParameters _ n = ""

--What determines if a function is private? 
isPubPriv :: Contract -> Int -> String
isPubPriv _ 0 = "public "


-- Creates the contract as a whole, first getting state variables and then fun/con
createContract :: Contract -> [[SolTypes]] -> String 
createContract c s= "pragma solidity ^0.5.0;" ++ "\n" ++ "contract Contract {" ++ "\n" ++ 
                  (getStVar (sortStateTypes c)) ++ "\n" ++
                  (getFunCon c) ++ "\n" ++
                  toString(createStandardFun s) ++ "\n" ++ "}"
                  

toFile :: String -> Contract -> IO ()
toFile s c = writeFile (s ++ "Solidity.sol") (createContract c (stateTypesToSort c))

getFunCon :: Contract -> String 
getFunCon c = toString(createFunCon c (stateTypesToSort c))

test = toFile "auctionTest" auction

testTypes = stateTypesToSort auction

commitEtherSolidity :: String -> Ether -> String
commitEtherSolidity "<" m = ("require(msg.value <" ++ show (m) ++ ");")
commitEtherSolidity ">" m = ("require(msg.value >" ++ show (m) ++ ");")
commitEtherSolidity "==" m = ("require(msg.value ==" ++ show (m) ++ ");")

sendAllSolidity :: String
sendAllSolidity = ("owner.transfer(this.balance);")