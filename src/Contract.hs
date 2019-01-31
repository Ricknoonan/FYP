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

newtype Person = Person Int  
    deriving(Show,Eq,Read)

type Money = Int 

type Owner = Person

--Record to keep track of contract state, people is an array of people involved in the contract
--etherBalance keeps track of money held in the contract at any stage of evaluation 
--owner is the person interacting with the contract

data State = State {
               people :: Map.Map Person Money,
               etherBalance  :: Money,
               owner :: Person
             }
             deriving (Eq,Show,Ord)

data InputState = InputState {
                    moneyIn :: Money,
                    decision :: Person,
                    nothing :: String
                    --date :: Observables
                    } 
             deriving (Eq,Show,Ord)

-- Initial starting point of contract evaluation with empty values. 
emptyState :: State
emptyState = State {people = Map.empty, etherBalance = 0, owner = 0}

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
                CashIn Money Person Contract Contract| -- allows a person to commit x amount that is defined the in the contract
                CashInUnlimited Money Person Contract Contract|
                CashOut Money Person Contract Contract|
                Pay Person Person Money Contract -- Pays person depening on event 
        deriving (Show, Eq)

-- Output of produced for each stage of evaluation e.g CommitPass means that someone has succesfully 
-- commited money to contract based on contract conditions 

data Output = Null |
              CommitFail Person Money |
              CommitPass Person Money |
              PayFail Person Person Money |
              PaySuccess Person Money |
              ObNotReached 
        deriving(Show)

-- Current observables that can be used to in a contract. Amount can be used to limit the amount in a contract

data Observables = Date (Integer, Int, Int) |
                   Amount Money
            deriving (Show, Eq)

-- ControlObs controls the observables and are evaluated to return boolean types to control if contract 
    --can progress in evaluation 
data ControlObs = NoOb |
                  AndOb Observables Observables |
                  OrOb Observables Observables |
                  Ob Observables
        deriving (Show, Eq)

type Decision = Person 

type OP = [Output]

--data In = Money | Decision

-- Takes a function, the current balance and the amount being committed and returns new balance
evalValue :: (Money -> Money -> Money) -> State -> Money -> Money 
evalValue f s val = f (etherBalance s) val 

-- evalC controls the contract evaluation
-- depending on the type of contract e.g cashIn or unitil, certain observables are checked and the next contract
-- to be evaluated, the updated state and the output at the statge are returned 
evalC :: InputState -> Contract -> ControlObs -> OP -> State -> (Contract, OP, State)
evalC inp c@(CashIn val person c1 c2) co o s 
    | val == inVal && evalObs co s inp = (c1, output, updateState)
    | otherwise = (c,[CommitFail person inVal], s)
        where
            newBal = evalValue (+) s inVal 
            os = people s
            output = [CommitPass person (inVal)]
            updateState = s {people = Map.insert person inVal os , etherBalance = newBal, owner = person}
            inVal = moneyIn inp 

evalC inp c@(When obs c1 c2) co o s
    | evalObs obs s inp = (c1, [], s)
    | otherwise = (c, [], s)

evalC inp c@(Pay person1 person2 val c1) co o s = 
    (c1, [PaySuccess (person1) val], s {etherBalance = newBal})
        where
            newBal = evalValue (-) s val 

evalC inp c@(Until obs c1 c2) co o s =  evalAll2 inp c1 obs o s

--TODO--
-- This initialises everyting when each new contract is returned, needs fixing 
evalAll :: Contract -> String -> (Contract, OP, State)
evalAll c@(CashIn val person c1 c2) inp = evalAll2  inVal c emptyOb [] emptyState
        where
            inVal = InputState { moneyIn = (read inp :: Money), decision = 0, nothing = "0"}

evalAll c@(Pay person1 person2 val c1) inp = evalAll2 inDecision c emptyOb [] emptyState
        where
            inDecision = InputState { moneyIn = 0, decision = Person (read inp :: Int), nothing = "0"}

evalAll c inp = evalAll2 noInput c emptyOb [] emptyState
        where 
            noInput = InputState {moneyIn = 0, decision = 0, nothing = "0"}

  
evalAll2 :: InputState -> Contract -> ControlObs -> OP -> State -> (Contract, OP, State)
evalAll2 inp c@(CashIn val person c1 c2) co o s  =  (nc, no, ns) 
        where
            (nc,no,ns) = evalC inp c co o s 
evalAll2 inp c@(Pay person1 person2 val c1) co o s = (nc, no, ns)
        where 
            (nc, no, ns) = evalC inp c co o s

evalAll2 inp c co o s = evalAll2 inp nc co no ns
        where
            (nc, no, ns) = evalC inp c co o s 

--Observables--

evalObs :: ControlObs -> State -> InputState -> Bool
evalObs (OrOb obs1 obs2) s inp = (checkObs obs1 s inp) || (checkObs obs2 s inp)
evalObs (AndOb obs1 obs2) s inp = (checkObs obs1 s inp) && (checkObs obs2 s inp)
evalObs (Ob obs1) s inp = checkObs obs1 s inp
evalObs (NoOb) s inp = True 

checkObs :: Observables -> State -> InputState -> Bool
checkObs (Date(y,m,d)) s inp = at (Date(y,m,d))
checkObs (Amount x) s inp = (etherBalance s + moneyIn inp) <= x

sameDate :: Observables -> Observables -> Bool
sameDate (Date (t1, t2, t3)) (Date (t4, t5, t6)) = 
    (t1 > t4 ) && (t2 > t5) && (t3 > t6)

at :: Observables -> Bool 
at tContract = sameDate tContract (Date (unsafePerformIO (todayDate)))

todayDate :: IO (Integer,Int,Int) -- :: (year,month,day)
todayDate = getCurrentTime >>= return . toGregorian . utctDay

instance Num Person where
    (Person x) + (Person y) = Person (x + y)
    (Person x) - (Person y) = Person (x - y)
    fromInteger x = Person (fromInteger x)

instance Ord Person where
    compare (Person x) (Person y) = compare x y


