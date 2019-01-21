module Contract where 

import Prelude hiding (and, Nothing)
import Numeric
import Data.Unique
import Data.Time
import Data.Map (Map, (!))
import qualified Data.Map as Map

{--
data Date = Date (Integer, Int, Int)
    deriving (Show, Eq)

currentTime :: IO (Integer,Int,Int) -- :: (year,month,day)
currentTime = getCurrentTime >>= return . toGregorian . utctDay

convertDate :: Date
convertDate = Date (y,m,d)
    where 
	(y,m,d) = currentTime

dateConverter :: Date -> IO (Integer, Int, Int)
dateConverter (Date (y,m,d)) = resultIO (y,m,d)

dateSame :: Day -> Day -> Bool
dateSame d1 d2 = d1 == d2
--}

data Person = Person Int  
    deriving(Show,Eq)

data Money = Money Int 
    deriving (Show,Eq)

data Owner = Owner Person
    deriving (Show,Eq)

--Record to keep track of contract state, people is an array of people involved in the contract, 
--owner is the "owner" of the contract
--meaning the person interacting with the contract(not currently functional)

data State = State {
               people :: Map.Map Person Money,
               etherBalance  :: Money,
               owner :: Person
             }
               deriving (Eq,Show,Ord)

emptyState :: State
emptyState = State {people = Map.empty, etherBalance = 0, owner = 0}

instance Num Money where
    (Money x) + (Money y) = Money (x + y)
    (Money x) - (Money y) = Money (x - y)
    fromInteger x = Money (fromInteger x)

instance Num Person where
	(Person x) + (Person y) = Person (x + y)
	(Person x) - (Person y) = Person (x - y)
        fromInteger x = Person (fromInteger x)

instance Ord Person where
    compare (Person x) (Person y) = compare x y

instance Ord Money where
    compare (Money x) (Money y) = compare x y

data Date = Date (Integer,Int,Int)
    deriving (Show, Eq)

today :: Date
today = Date (2018,12,14)

data Contract = End |
                Time Date Contract Contract| 
                Scale Double Contract |
                Give Contract | 
                And Contract Contract |
                Or Contract Contract |
                Until Date Contract Contract|
                CashIn Money Person Contract Contract|
                CashOut Money Person Contract Contract|
                When Date Contract |
                Pay Person Person Money Contract
        deriving (Show, Eq)

data ReadableContract = Empty String |
                        Payout Double |
                        Send ReadableContract |
                        Expired (Obs Bool) |
                        BetterContract ReadableContract  |
                        Join ReadableContract ReadableContract |
                        TimeReadable ReadableContract ReadableContract         
        deriving (Show)

data Output = Null |
              CommitFail Person Money |
              CommitPass Person Money |
              PayFail Person Person Money |
              PaySuccess Person Person Money 
        deriving(Show)

newtype Obs a = Obs (Date -> a)

instance Show a => Show (Obs a) where
   show (Obs o) = show  (o today)            

type OP = [Output]

evalValue ::(Money -> Money -> Money) -> State -> Money -> Money 
evalValue f s val = f (etherBalance s) val 

evalC :: Contract -> State -> (Contract, OP, State)
evalC c@(CashIn val person c1 c2) s = 
	(c1, [CommitPass person (val)], s {people = Map.insert person val os, etherBalance = newBal, owner = person})
        where 
            newBal = evalValue (+) s val 
            os = people s

evalC c@(Time date c1 c2) s
    | (at date) = (c1, [], s)
    | otherwise = (c2, [], s)

evalC c@(Pay person1 person2 val c1) s = 
	(c1, [PaySuccess person1 person2 val], s {etherBalance = newBal})
        where
            newBal = evalValue (-) s val 

evalC c@(Until date c1 c2) s
    | (at date) = (c2, [Null], s)
    | otherwise = (c1, [], s)

evalAll :: Contract -> (Contract, OP, State)
evalAll c = evalAll2 c [] emptyState

evalAll2 :: Contract -> OP -> State -> (Contract, OP, State)
evalAll2 c o s
    | c == End = (c, o, s)
    | otherwise = evalAll2 nc (o ++ no) ns
        where
            (nc, no,ns) = evalC c s

--Observables--

--Constant to scale contract 
konst :: a -> Obs a
konst k = Obs (\t -> k)

--Checks if time horizon has been reached
sameDate :: Date -> Date -> Bool
sameDate (Date (t1, t2, t3)) (Date (t4,t5,t6)) = 
    (t1 == t4) && (t2==t5) && (t3==t6)

at :: Date -> Bool 
at t_future = sameDate t_future today