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
        ("timelimit") -> (Set (TimeLimit) (readDataTypes xs))
        ("totalamount") -> (Set (TotalAmount) (readDataTypes xs))
        ("beneficiary") -> (Set (Beneficiary) (readDataTypes xs))
        _ -> (Error ("Incorrect Parameter for Set: " ++ x))
readDataTypes ("commitether":x:y:xs) = 
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
        ("higherthan") -> (CommitEther (HigherThan y) (readDataTypes xs))
        _ -> (Error ("Incorrect Parameter for CommitEther: " ++ x))
readDataTypes ("when":x:y:xs) = 
    case x of 
        ("amount") 
            | ((readInt y) < 0) -> (Error "Number must be greater than 0 for When Amount")
            | otherwise -> (When (Amount (readMoney y)) (readDataTypes (xs)))
        ("people")
            | ((readInt y) < 0) -> (Error "Number must be greater than 0 for When People")
            | otherwise -> (When (People (readInt y)) (readDataTypes (xs)))
        ("totalreached") -> (When (TotalReached) (readDataTypes (y:xs)))
        ("timesup") -> (When (TimesUp) (readDataTypes (y:xs)))
        ("totalamount") -> (When (TotalAmount) (readDataTypes (y:xs)))
        _ -> (Error ("Incorrect Parameter for Contract When"))
readDataTypes ("send":x:y:xs) =
    case x of
        ("beneficiary") -> case y of 
            ("all") -> (Send (ToBeneficiary All) (readDataTypes xs))
            ("rest") -> (Send (ToBeneficiary Rest) (readDataTypes xs))
            ("amountin") -> (Send (ToBeneficiary (AmountIn (getString xs))) (readDataTypes (y:xs)))
            ("partial")
                | (0<(readDouble xs) && (readDouble xs)<1 ) -> (Send (ToBeneficiary (Partial (readDouble xs))) (readDataTypes xs))
                | otherwise -> patialError
            _ -> Error ("Incorrect Parameter for Beneficiary")  
        ("winner") -> case y of
            ("all") -> (Send (Winner All) (readDataTypes xs))
            ("rest") -> (Send (Winner Rest) (readDataTypes xs))
            ("amountin") -> (Send (Winner (AmountIn (getString xs))) (readDataTypes xs))
            ("partial")
                | (0<(readDouble xs) && (readDouble xs)<1 ) -> (Send (Winner (Partial (readDouble xs))) (readDataTypes xs))
                | otherwise -> patialError
            _ -> Error ("Incorrect Parameter for Winner")        
        ("random") -> case y of 
            ("all") -> (Send (Random All) (readDataTypes xs))
            ("rest") -> (Send (Random Rest) (readDataTypes xs))
            ("amountin") -> (Send (Random (AmountIn (getString xs))) (readDataTypes xs))
            ("partial")
                | (0<(readDouble xs) && (readDouble xs)<1 ) -> (Send (Random (Partial (readDouble xs))) (readDataTypes xs))
                | otherwise -> patialError
            _ -> Error ("Incorrect Parameter for Random")
        ("owner") -> case y of 
            ("all") -> (Send (ToOwner All) (readDataTypes xs))
            ("rest") -> (Send (ToOwner Rest) (readDataTypes xs))
            ("amountin") -> (Send (ToOwner (AmountIn (getString xs))) (readDataTypes xs))
            ("partial")
                | (0<(readDouble xs) && (readDouble xs)<1 ) -> (Send (ToOwner (Partial (readDouble xs))) (readDataTypes xs))
                | otherwise -> patialError
            _ -> Error ("Incorrect Parameter for ToOwner") 
        ("address") -> case y of 
            ("all") -> (Send (Address All) (readDataTypes xs))
            ("rest") -> (Send (Address Rest) (readDataTypes xs))
            ("amountin") -> (Send (Address (AmountIn (getString xs))) (readDataTypes xs))
            ("partial")
                | (0<(readDouble xs) && (readDouble xs)<1 ) -> (Send (Address (Partial (readDouble xs))) (readDataTypes xs))
                | otherwise -> patialError
            _ -> Error ("Incorrect Parameter for Address") 

        _ -> (Error ("Incorrect Parameter for Contract Send"))
readDataTypes ("partial":xs) = readDataTypes xs
readDataTypes ("amountin":x:xs) = readDataTypes (xs)
readDataTypes ("until":x:y:xs) = 
    case x of 
        ("amount") 
            | ((readInt y) < 0) -> (Error "Number must be greater than 0 for Until Amount")
            | otherwise -> (Until (Amount (readMoney y)) (readDataTypes (xs)))
        ("people")
            | ((readInt y) < 0) -> (Error "Number must be greater than 0 for Until People")
            | otherwise -> (Until (People (readInt y)) (readDataTypes (xs)))
        ("totalreached") -> (Until (TotalReached) (readDataTypes (y:xs)))
        ("timesup") -> (Until (TimesUp) (readDataTypes (y:xs)))
        ("totalamount") -> (Until (TotalAmount) (readDataTypes (y:xs)))
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
        ("alreadyfinished") -> (Unless (AlreadyFinished)(readDataTypes xs))
        _ -> (Error ("Incorrect Parameter for Unless " ++ x))

readDataTypes ("=":xs) = readDataTypes xs

readDataTypes (x:xs) = (Error x)



getString :: [String] -> String
getString (x:xs) = x

patialError :: Contract 
patialError = (Error ("Number must be 0 < x > 1 for Partial"))

readInt :: String -> Int 
readInt s = (read s :: Int)

readDouble :: [String] -> Double 
readDouble (x:xs) = (read x :: Double)

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
            ("=") -> checkConstuctor (y:z:xs)
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
        | (isSend(x:xs)) = (isCommitEther(x:xs))
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