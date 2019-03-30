module ToContract where

import Contract 
import ContractClass
import Simulate
import ToSolidity

readDataTypes :: [String] -> Contract
readDataTypes [] = End  

readDataTypes ("function":x:xs)
    | (x == "") = (Error "Function must have a name")
    | otherwise = (Function x (readDataTypes xs))

readDataTypes ("constructor":xs) = (Constructor (readDataTypes xs))

readDataTypes ("set":x:xs) = 
    case x of 
        ("contractowner") -> (Set (ContractOwner) (readDataTypes xs))
        ("timelimit") -> (Set (ContractOwner) (readDataTypes xs))
        ("totalamount") -> (Set (TotalAmount) (readDataTypes xs))
        _ -> (Error ("Incorrect Parameter for Set: " ++ x))
 
readDataTypes ("CommitEther":x:y:xs) = 
    case x of
        ("nolimit") -> (CommitEther (NoLimit) (readDataTypes (y:xs)))
        ("equal") 
            | ((readInt y)<0 ) -> (Error "Number must be greater than 0 for CommitEther Equal")
            | otherwise -> (CommitEther (Equal (readMoney y)) (readDataTypes (xs)))
        ("min")
            | ((readInt y) <= 0) -> (Error "Number must be greater than or equal to 0")
            | otherwise -> (CommitEther (Min (readMoney y)) (readDataTypes (xs)))
        ("max")
            | ((readInt y) <= 0) -> (Error "Number must be greater than or equal to 0")
            | otherwise -> (CommitEther (Max (readMoney y)) (readDataTypes (xs)))
        _ -> (Error ("Incorrect Parameter for CashIn: " ++ x))

readDataTypes ("when":x:y:xs) = 
    case x of 
        ("amount") 
            | ((readInt y) < 0) -> (Error "Number must be greater than 0 for When Amount")
            | otherwise -> (When (Amount (readMoney y)) (readDataTypes (xs)))
        ("people")
            | ((readInt y) < 0) -> (Error "Number must be greater than 0 for When People")
            | otherwise -> (When (People (readInt y)) (readDataTypes (xs)))
        ("totalreached") -> (When (TotalReached) (readDataTypes (xs)))
        ("timesup") -> (When (TimesUp) (readDataTypes (xs)))
        ("totalamount") -> (When (TotalAmount) (readDataTypes (xs)))
        _ -> (Error ("Incorrect Parameter for Contract When"))

readDataTypes ("send":x:y:xs) =
    case x of
        ("winner") -> case y of
            ("all") -> (Send (Winner All) (readDataTypes xs))
            ("rest") -> (Send (Winner Rest) (readDataTypes xs))
            _ -> Error ("Incorrect Parameter for Winner")
            
        ("random") -> case y of 
            ("all") -> (Send (Random All) (readDataTypes xs))
            ("rest") -> (Send (Random Rest) (readDataTypes xs))
            _ -> Error ("Incorrect Parameter for Random")

        ("toowner") -> case y of 
            ("all") -> (Send (ToOwner All) (readDataTypes xs))
            ("rest") -> (Send (ToOwner Rest) (readDataTypes xs))
            _ -> Error ("Incorrect Parameter for ToOwner")
            
        ("beneficiary") -> case y of 
            ("all") -> (Send (Beneficiary All) (readDataTypes xs))
            ("rest") -> (Send (Beneficiary Rest) (readDataTypes xs))
            _ -> Error ("Incorrect Parameter for Beneficiary")   
        _ -> (Error ("Incorrect Parameter for Contract Send"))

readDataTypes ("send":x:y:z:xs) = 
    case x of 
        ("winner") -> case y of
            ("partial")
                | (0<(readDouble z) && (readDouble z)<1 ) -> (Send (Winner (Partial (readDouble z))) (readDataTypes xs))
                | otherwise -> (Error ("Number must be 0 < x > 1 for Partial"))
            _ -> Error ("Incorrect Parameter for Winner") 
        ("random") -> case y of
            ("partial")
                | (0<(readDouble z) && (readDouble z)<1 ) -> (Send (Random (Partial (readDouble z))) (readDataTypes xs))
                | otherwise -> (Error ("Number must be 0 < x > 1 for Partial"))
            _ -> Error ("Incorrect Parameter for Random")
        ("toowner") -> case y of
            ("partial")
                | (0<(readDouble z) && (readDouble z)<1 ) -> (Send (ToOwner (Partial (readDouble z))) (readDataTypes xs))
                | otherwise -> (Error ("Number must be 0 < x > 1 for Partial"))
            _ -> Error ("Incorrect Parameter for ToOwner")
        ("beneficiary") -> case y of
            ("partial")
                | (0<(readDouble z) && (readDouble z)<1 ) -> (Send (Beneficiary (Partial (readDouble z))) (readDataTypes xs))
                | otherwise -> (Error ("Number must be 0 < x > 1 for Partial"))
            _ -> Error ("Incorrect Parameter for Beneficiary")
        _ -> (Error ("Incorrect Parameter for Contract Send")) 

readDataTypes ("until":x:y:xs) = 
    case x of 
        ("amount") 
            | ((readInt y) < 0) -> (Error "Number must be greater than 0 for Until Amount")
            | otherwise -> (Until (Amount (readMoney y)) (readDataTypes (xs)))
        ("people")
            | ((readInt y) < 0) -> (Error "Number must be greater than 0 for Until People")
            | otherwise -> (Until (People (readInt y)) (readDataTypes (xs)))
        ("totalreached") -> (Until (TotalReached) (readDataTypes (xs)))
        ("timesup") -> (Until (TimesUp) (readDataTypes (xs)))
        ("totalamount") -> (Until (TotalAmount) (readDataTypes (xs)))
        _ -> (Error ("Incorrect Parameter for Contract Until"))

readDataTypes ("addto":x:xs) = 
    case x of
        "" -> (Error ("AddTo must have a name"))
        _ -> (AddTo x (readDataTypes xs))

readDataTypes ("from": x:xs) = 
    case x of 
        "" -> (Error ("From must have a name"))
        _ -> (From x (readDataTypes xs))

readDataTypes ("withdraw": xs) = (Withdraw (readDataTypes xs)) 

readDataTypes ("isnot":xs) = (IsNot (readDataTypes xs))

readDataTypes ("unless":x:xs) =
    case x of 
        ("alreadyjoined") -> (Unless (AlreadyJoined) (readDataTypes xs))
        _ -> (Error ("Incorrect Parameter for Unless " ++ x))

readDataTypes (x:xs) = (Error x) 


readInt :: String -> Int 
readInt s = (read s :: Int)

readDouble :: String -> Double 
readDouble s = (read s :: Double)

readMoney :: String -> Ether 
readMoney s = (read s :: Ether)

checkWithdraw :: [String] -> Bool 
checkWithdraw (x:xs) 
        | (isWithdraw (x:xs)) = (isAddTo (x:xs)) && (isCommitEther (x:xs))
        | otherwise = True 

checkConstuctor :: [String] -> Bool
checkConstuctor (x:y:z:xs) = if (isConstructor (x:xs)) then
    case x of 
        ("constructor") -> case y of 
            ("set") -> case z of 
                ("contractowner") -> True
                ("timelimit") -> True
                ("totalamount") -> True
                _ -> False
            _ -> False
        _ -> checkConstuctor (y:z:xs)
    else True
checkConstuctor [] = True


checkPartialRest :: [String] -> Bool
checkPartialRest (x:xs) 
        |(isPartial (x:xs)) = (isRest (x:xs))
        | otherwise = True

checkSend :: [String] -> Bool
checkSend (x:xs) 
        | (isSend(x:xs)) = (isCashIn(x:xs))
        | otherwise = True

checkCommitEther :: [String] -> Bool 
checkCommitEther (x:xs) 
        | (isCommitEther (x:xs)) = ((isWithdraw (x:xs)) || (isSend (x:xs))) 
        | otherwise = True

checkAddTo :: [String] -> Bool 
checkAddTo (x:xs) = if (isAddTo (x:xs)) then 
    case x of
    ("cashin")  
        | (checkAddToHelper xs) -> True
        | otherwise -> False
    _ -> checkAddTo xs
    else True
checkAddTo [] = True

checkAddToHelper :: [String] -> Bool 
checkAddToHelper (x:xs) = 
    case x of 
        ("addto") -> True
        ("function") -> False
        _ -> checkAddToHelper xs 
checkAddToHelper [] = False

isWithdraw :: [String] -> Bool
isWithdraw [] = False 
isWithdraw ("withdraw":xs) = True
isWithdraw (x:xs) = isWithdraw xs 

isAddTo :: [String] -> Bool
isAddTo  [] = False 
isAddTo ("addto":xs) = True
isAddTo (x:xs) = isAddTo xs 

isCommitEther :: [String] -> Bool
isCommitEther  [] = False 
isCommitEther ("commitether":xs) = True
isCommitEther (x:xs) = isCommitEther xs

isSet :: [String] -> Bool
isSet  [] = False 
isSet ("set":xs) = True
isSet (x:xs) = isSet xs

isPartial :: [String] -> Bool
isPartial  [] = False 
isPartial ("partial":xs) = True
isPartial (x:xs) = isPartial xs 

isRest :: [String] -> Bool
isRest  [] = False 
isRest ("rest":xs) = True
isRest (x:xs) = isRest xs 

isSend :: [String] -> Bool
isSend  [] = False 
isSend ("send":xs) = True
isSend (x:xs) = isSend xs

isConstructor :: [String] -> Bool
isConstructor  [] = False 
isConstructor ("constructor":xs) = True
isConstructor (x:xs) = isConstructor xs 


-- Withdraw needs to have an addto and cashin
-- set needs to be in a contstructor 
-- if partial is used then there needs to be a rest
-- send needs to have a CommitEther
-- addto needs to have a CommitEther

--TODO
-- owner needs to be set if anythingis happening with the owner