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

newtype Owner = Owner Person
    deriving (Show,Eq,Read)

--Record to keep track of contract state, people is an array of people involved in the contract, 
--owner is the person interacting with the contract

data State = State {
               people :: Map.Map Person Money,
               etherBalance  :: Money,
               owner :: Person
             }
               deriving (Eq,Show,Ord)

emptyState :: State
emptyState = State {people = Map.empty, etherBalance = 0, owner = 0}

emptyOb :: ControlObs
emptyOb = NoOb

emptyInp :: Money 
emptyInp = 0

data Contract = End |
                When ControlObs Contract Contract| 
                Scale Double Contract |
                Give Contract | 
                And Contract Contract |
                Or Contract Contract |
                Until ControlObs Contract Contract|
                CashIn Money Person Contract Contract|
                CashInUnlimited Money Person Contract Contract|
                CashOut Money Person Contract Contract|
                Pay Person Person Money Contract
        deriving (Show, Eq)

data Output = Null |
              CommitFail Person Money |
              CommitPass Person Money |
              PayFail Person Person Money |
              PaySuccess Person Person Money |
              ObNotReached 
        deriving(Show)

data Observables = Date (Integer, Int, Int) |
                   Amount Money
            deriving (Show, Eq)

data ControlObs = NoOb |
                  AndOb Observables Observables |
                  OrOb Observables Observables |
                  Ob Observables
        deriving (Show, Eq)

type MoneyIn = Int 

type OP = [Output]

evalValue :: (Money -> Money -> Money) -> State -> Money -> Money 
evalValue f s val = f (etherBalance s) val 

evalC :: MoneyIn -> Contract -> ControlObs -> OP -> State -> (Contract, OP, State)
evalC inp c@(CashIn val person c1 c2) co o s 
    | val == inp && evalObs co s inp = (c1, output, updateState)
    | otherwise = (c,[CommitFail person inp], s)
        where
            newBal = evalValue (+) s inp 
            os = people s
            output = [CommitPass person (inp)]
            updateState = s {people = Map.insert person inp os , etherBalance = newBal, owner = person}

evalC inp c@(When obs c1 c2) co o s
    | evalObs obs s inp = (c1, [], s)
    | otherwise = (c, [], s)
         --where
           -- (nc, no, ns) = (c1, [], s)

evalC inp c@(Pay person1 person2 val c1) co o s = 
	(c1, [PaySuccess person1 person2 val], s {etherBalance = newBal})
        where
            newBal = evalValue (-) s val 

evalC inp c@(Until obs c1 c2) co o s =  evalAll2 inp c1 obs o s

evalAll :: Contract -> (Contract, OP, State)
evalAll c = evalAll2 emptyInp c emptyOb [] emptyState 

evalAll2 :: MoneyIn -> Contract -> ControlObs -> OP -> State -> (Contract, OP, State)
evalAll2 inp c@(CashIn val person c1 c2) co o s  =  (nc, no, ns) {--evalAll2 inp nc co no ns--}
        where
            (nc,no,ns) = evalC (input val person) c co o s 

evalAll2 inp c co o s = evalAll2 inp nc co no ns
        where
            (nc, no, ns) = evalC inp c co o s 

input2 :: Money -- Hard coded input 
input2 = 20

input :: Money -> Person -> Money
input m p = (unsafePerformIO $ getIO m p)

getIO :: Money -> Person -> IO (Money)
getIO m p = do 
    putStrLn "Enter amount  " >> putStrLn (show m) >> putStrLn (show p)
    line <- getLine 
    return (read line :: Money)

--Observables--

evalObs :: ControlObs -> State -> MoneyIn-> Bool
evalObs (OrOb obs1 obs2) s inp = (checkObs obs1 s inp) || (checkObs obs2 s inp)
evalObs (AndOb obs1 obs2) s inp = (checkObs obs1 s inp) && (checkObs obs2 s inp)
evalObs (Ob obs1) s inp = checkObs obs1 s inp
evalObs (NoOb) s inp = True 

checkObs :: Observables -> State -> MoneyIn -> Bool
checkObs (Date(y,m,d)) s inp = at (Date(y,m,d))
checkObs (Amount x) s inp = (etherBalance s + inp) <= x

sameDate :: Observables -> Observables -> Bool
sameDate (Date (t1, t2, t3)) (Date (t4, t5, t6)) = 
    (t1 > t4 ) && (t2 > t5) && (t3 > t6)

at :: Observables -> Bool 
at tContract = sameDate tContract today

today :: Observables
today = Date (2018,12,12)

---------------------------
--Annex--
--------------------------

{--
evalOnce :: Contract -> OP -> State -> (Contract,OP,State)
evalOnce c o s = (nc,no,ns)
    where 
        (nc,no,ns) = evalC c s 

--    | (at obs) = (c2, cp {date = },[Null], s) -- evalObs to return a bool here 
--    | otherwise = (c1, [], s)
--}
instance Num Person where
    (Person x) + (Person y) = Person (x + y)
    (Person x) - (Person y) = Person (x - y)
    fromInteger x = Person (fromInteger x)

instance Ord Person where
    compare (Person x) (Person y) = compare x y


