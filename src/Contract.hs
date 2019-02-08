module Contract where 

import Prelude hiding (and)
import Numeric
import Data.Unique
import Data.Time
import Data.Map (Map, (!))
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import System.Console.Haskeline
import Control.Monad.Trans.Except 

type Person = Address  

type Money = Int 

type Owner = Person

type Address = String

data Action =  Commit Person Money |
               PayOut Person Money 
    deriving (Eq,Show,Ord)

--Record to keep track of contract state, people is an array of people involved in the contract
--etherBalance keeps track of money held in the contract at any stage of evaluation 
--owner is the person interacting with the contract

data State = State {
               commits :: Map.Map Int Action,
               etherBalance  :: Money,
               owner :: Person
             }
             deriving (Eq,Show,Ord)

data InputState = InputState {
                    moneyIn :: Money,
                    decision :: Int,
                    nothing :: String
                    } 
             deriving (Eq,Show,Ord)

-- Initial starting point of contract evaluation with empty values. 
emptyState :: State
emptyState = State {commits = Map.empty, etherBalance = 0, owner = "0"}

emptyOb :: ControlObs
emptyOb = NoOb

emptyInput :: InputState
emptyInput = InputState { moneyIn = 0, decision = 0, nothing = "0"}

-- Contract data types 

data Contract = End |
                When ControlObs Contract Contract| -- When observable is true, next action can happen
                Scale Double Contract |
                Give Contract | 
                And Contract Contract|
                Or Contract Contract |
                Until ControlObs Contract Contract| -- until a certain observable, the following contracts can be evaluated
                CashIn Money Address Contract Contract Contract| -- allows a person to commit x amount that is defined the in the contract
                CashInUnlimited Address Contract Contract Contract|
                CashOut Contract Contract|
                Pay ControlObs Contract| -- Pays person depening on event 
                People Int 
        deriving (Show, Eq)

-- Output of produced for each stage of evaluation e.g CommitPass means that someone has succesfully 
-- commited money to contract based on contract conditions 

data Output = Null |
              CommitFail Action |
              CommitPass Action |
              PayFail Action |
              PaySuccess Action |
              CashRedeemed [Action] |
              ObNotReached 
        deriving(Show)

-- Current observables that can be used to in a contract. Amount can be used to limit the amount in a contract

data Observables = Date (Integer, Int, Int) |
                   Amount Money |
                   Winner Address |
                   Highest Address |
                   Random Address
            deriving (Show, Eq)

-- ControlObs controls the observables and are evaluated to return boolean types to control if contract 
    --can progress in evaluation 
data ControlObs = NoOb |
                  BothOb Observables Observables |
                  EitherOb Observables Observables |
                  Ob Observables
        deriving (Show, Eq)

type Decision = Person 

type OP = [Output]

-- Takes a function, the current balance and the amount being committed and returns new balance
evalValue :: (Money -> Money -> Money) -> State -> Money -> Money 
evalValue f s val = f (etherBalance s) val 

-- evalC controls the contract evaluation
-- depending on the type of contract e.g cashIn or unitil, certain observables are checked and the next contract
-- to be evaluated, the updated state and the output at the statge are returned 

evalC :: InputState -> Contract -> ControlObs -> OP -> State -> (Contract, OP, State, ControlObs)
evalC inp c@(CashIn val address (People p) c1 c2) co o s 
    | val == inVal && evalObs co s inp && (size s < p) = (c, output, updateState, co)
    | not (evalObs co s inp) || (size s == p) = (c1, [CommitFail (Commit address inVal)], s, co)
    | otherwise = (c,[CommitFail (Commit address inVal)], s, co)
        where
            newBal = evalValue (+) s inVal 
            os = commits s
            output = [CommitPass (Commit address inVal)]
            updateState = s {commits = Map.insert (index p s) (Commit address inVal) os, etherBalance = newBal, owner = address}
            inVal = moneyIn inp 

evalC inp c@(When obs c1 c2) co o s
    | evalObs obs s inp = (c1, [], s, co)
    | otherwise = (c, [], s, co)

evalC inp c@(Pay obs c1) co o s = (c1, [PaySuccess (PayOut x payAmount)], s {etherBalance = newBal}, co)
        where
            payAmount = etherBalance s 
            newBal = evalValue (-) s payAmount 
            (x:xs) = evalPay obs s inp 

evalC inp c@(CashOut c1 c2) co o s 
    | evalObs co s inp = (c2, o, updateState, co)
    | (size s) > 0 = evalC inp c co (o ++ [no]) updateState
    | otherwise = evalC inp c1 co o s
        where 
            no = CashRedeemed (findAtIndex ([size s]) s)
            newCommits = Map.delete (size s) (commits s)
            currentBal = etherBalance s
            refund = commitAtIndex (size s) s
            updateState = s {commits = newCommits, etherBalance = (currentBal - refund), owner = ""}

--evalC inp c@(CashInUnlimited )

evalC inp c@(Until obs c1 c2) co o s =  evalAll2 inp c1 obs o s

run :: Contract -> String -> ControlObs -> State -> (Contract, OP, State, ControlObs)
run c@(CashIn val address people c1 c2) inp co s = evalAll2 inVal c co [] s
        where
            inVal = InputState { moneyIn = (read inp :: Money), decision = 0, nothing = "0"}

run c@(Pay address c1) inp co s = evalAll2 inDecision c co [] s
        where
            inDecision = InputState { moneyIn = 0, decision = (read inp :: Int), nothing = "0"}

run c inp co s = evalAll2 noInput c co [] s
        where 
            noInput = InputState {moneyIn = 0, decision = 0, nothing = "0"}


evalAll2 :: InputState -> Contract -> ControlObs -> OP -> State -> (Contract, OP, State, ControlObs)
evalAll2 inp c@(CashIn val address people c1 c2) co o s  =  (nc, no, ns, nco) 
        where
            (nc, no, ns, nco) = evalC inp c co o s 
evalAll2 inp c@(Pay address c1) co o s = (nc, no, ns, nco)
        where 
            (nc, no, ns, nco) = evalC inp c co o s

evalAll2 inp c@(Until obs c1 c2) co o s = evalAll2 inp nc nco no ns
        where
            (nc, no, ns, nco) = evalC inp c co o s 

evalAll2 inp c co o s = evalAll2 inp nc co no ns
        where
            (nc, no, ns, nco) = evalC inp c co o s 

evalPay :: ControlObs -> State -> InputState -> [Address]
evalPay (Ob (Highest addr)) s inp = getAddress (findAtIndex (highestInMap (commits s)) s)
evalPay (Ob (Winner addr)) s inp = getAddress (findAtIndex([decision inp]) s)

evalObs :: ControlObs -> State -> InputState -> Bool
evalObs (EitherOb obs1 obs2) s inp = (checkObs obs1 s inp) || (checkObs obs2 s inp)
evalObs (BothOb obs1 obs2) s inp = (checkObs obs1 s inp) && (checkObs obs2 s inp)
evalObs (Ob obs1) s inp = checkObs obs1 s inp
evalObs (NoOb) s inp = True 

checkObs :: Observables -> State -> InputState -> Bool
checkObs (Date(y,m,d)) s inp = at (Date(y,m,d))
checkObs (Amount x) s inp = (etherBalance s + moneyIn inp) <= x

sameDate :: Observables -> Observables -> Bool
sameDate (Date (t1, t2, t3)) (Date (t4, t5, t6)) 
    | (t1 > t4 ) = False 
    | (t1 > t4 ) && (t2 > t5) = False
    | (t1 > t4 ) && (t2 > t5) && (t3 > t6) = False
    | otherwise = True

at :: Observables -> Bool 
at tContract = sameDate tContract (Date (unsafePerformIO (todayDate)))

todayDate :: IO (Integer,Int,Int) -- :: (year,month,day)
todayDate = getCurrentTime >>= return . toGregorian . utctDay

index :: Int -> State -> Int 
index p s
    | Map.member p (commits s) = index (p-1) s
    | otherwise = p 

size :: State -> Int 
size s = Map.size (commits s)

getMoneyCommit :: [Action] -> Int 
getMoneyCommit [(Commit p m)] = m

getAddress :: [Action] -> [Address]
getAddress [(Commit p m)] = [p]
getAddress (Commit p m:rest) = p:getAddress rest

commitAtIndex :: Int -> State -> Int
commitAtIndex ind s = getMoneyCommit (findAtIndex [ind] s)

findAtIndex :: [Int] -> State -> [Action]
findAtIndex [] s = [(Commit "No Commit" 0)]
findAtIndex [i] s = [(Map.findWithDefault (Commit "No Commit" 0) (i) (commits s))]
findAtIndex (i:is) s = (Map.findWithDefault (Commit "No Commit" 0) (i) (commits s)):findAtIndex is s

address :: Address 
address = " "

highestInMap :: Map Int Action -> [Int]
highestInMap m = go [] Nothing (Map.toList m)
  where
    go ks _        []           = ks 
    go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | getMoneyCommit [v] < getMoneyCommit [u] = go ks (Just u) rest
        | getMoneyCommit [v] > getMoneyCommit [u] = go [k] (Just v) rest
        | otherwise = go (k:ks) (Just v) rest
