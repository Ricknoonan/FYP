module Contract where 

import Prelude hiding (and, Nothing)
import Numeric
import Data.Unique
import Data.Time
import Control.Monad.Trans.State.Strict

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


data ContractSt = ContractSt { people :: [Person], etherBalance :: Money, owner :: Person } deriving (Show)

updateState :: (Money -> Money -> Money) -> Money -> Person -> State ContractSt ()
updateState f val per = do
    cs <- get 
    let bal = (etherBalance cs) + (val) 
    let person = people cs ++ [per]
    put $ cs { people = person, etherBalance = bal, owner = per}

initialState :: State ContractSt ()
initialState = do 
    cs <- get 
    put $ cs {people = [1], etherBalance = 0, owner = 0}

instance Num Money where
    (Money x) + (Money y) = Money (x + y)
    (Money x) - (Money y) = Money (x - y)
    fromInteger x = Money (fromInteger x)

instance Num Person where
	(Person x) + (Person y) = Person (x + y)
	(Person x) - (Person y) = Person (x - y)
        fromInteger x = Person (fromInteger x)

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

evalC :: Contract -> State ContractSt () -> (Contract, OP, State ContractSt ())
evalC c@(CashIn val person c1 c2) s = 
	(c1, [CommitPass person (val)], updateState (+) val person)

evalC c@(Time date c1 c2) s
    | (at date) = (c1, [], s)
    | otherwise = (c2, [], s)

evalC c@(Pay person1 person2 val c1) s = 
	(c1, [PaySuccess person1 person2 val], updateState (-) val person1)

evalC c@(Until date c1 c2) s
    | (at date) = (c2, [Null], s)
    | otherwise = (c1, [], s)

evalAll :: Contract -> (Contract, OP, ContractSt)
evalAll c = execLast3 (evalAll2 c [] initialState)

evalAll2 :: Contract -> OP -> State ContractSt () -> (Contract, OP, State ContractSt ())
evalAll2 c o s
    | c == End = (c, o, s)
    | otherwise = evalAll2 nc (o ++ no) ns
        where
            (nc, no,ns) = evalC c s

execLast3 :: (Contract, OP, State ContractSt ()) -> (Contract, OP, ContractSt)
execLast3 f = maplast3 myExecuter f

myExecuter :: State ContractSt () -> (ContractSt)
myExecuter f = execState (f) (ContractSt {people = [], etherBalance = 0 , owner = 0 })

maplast3 :: (x -> c) -> (a,b,x) -> (a,b,c)
maplast3 f (a,b,x) = (a, b, f x)

--Observables--

--Constant to scale contract 
konst :: a -> Obs a
konst k = Obs (\t -> k)

--Checks if time horizon has been reached
sameDate :: Date -> Date -> Bool
sameDate (Date (t1, t2, t3)) (Date (t4,t5,t6)) = 
    (t1 == t4) && (t2==t3) && (t3==t6)

at :: Date -> Bool 
at t_future = sameDate t_future today