module Contract where 

import Prelude hiding (and, Nothing)
import Numeric
import Data.Unique
import Data.Time
import Control.Monad.Trans.State.Lazy

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
{--
data ContactSt = ContactSt { 
    people :: People [Person],
    etherBalance :: EtherBalance Money,
    owner :: Owner Person
}
--}
data ContactSt = People Person |
                 EtherBalance Money |
                 Owner Int 
        deriving (Show)

addToState :: Money -> Person -> State ContactSt ()
addToState val p = do 
	(EtherBalance bal) <- get
	put $ EtherBalance (bal + val)
	(People person) <- get
	put $ People p
	(Owner owner) <- get 
	put $ Owner p

takeFromState :: Money -> State ContactSt ()
takeFromState val = do 
	(EtherBalance bal) <- get 
	put $ EtherBalance (bal - val)

getValue :: State ContactSt (Person, Money, Int)
getValue = do
	(People person) <- get
	(EtherBalance bal) <- get
	(Owner owner) <- get 
	pure (person, bal, owner)

initialState :: State ContactSt ()
initialState = do 
	(Owner owner) <- get
	let owner = 0
	put $ Owner owner
	(People people) <- get 
	put $ People 0
	(EtherBalance bal) <- get
	let bal = 0
	put $ EtherBalance bal
{--
program :: State ContactSt (Person, Money, Int)
program = do
  addToState 10 1
  takeFromState 4
  getValue

runProgram :: (Person, Money, Int)
runProgram = evalState program (EtherBalance 0)
--}
data Date = Date (Integer,Int,Int)
    deriving (Show, Eq)

today :: Date
today = Date (2018,12,14)

type Person = Int

type Money = Int

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

evalC :: Contract -> State ContactSt () -> (Contract, OP, State ContactSt ()) 
evalC c@(CashIn val person c1 c2) s = 
	(c1, [CommitPass person (val)], addToState val person)

evalC c@(Time date c1 c2) s
    | (at date) = (c1, [], s)
    | otherwise = (c2, [], s)

evalC c@(Pay person1 person2 val c1) s = 
	(c1, [PaySuccess person1 person2 val], takeFromState val)

evalC c@(Until date c1 c2) s
    | (at date) = (c2, [Null], s)
    | otherwise = (c1, [], s)

evalAll :: Contract -> (Contract, OP, State ContactSt ())
evalAll c = evalAll2 c [] initialState

evalAll2 :: Contract -> OP -> State ContactSt () -> (Contract, OP, State ContactSt ())
evalAll2 c o s
    | c == End = (c, o, s)
    | otherwise = evalAll2 nc (o ++ no) ns
        where
            (nc, no, ns) = evalC c s
       
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