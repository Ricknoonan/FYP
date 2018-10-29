module Contract where 

import Numeric
import Control.Monad
import Data.Unique
import Data.Time

data CurrencySymbol = EUR | USD | GBP
      deriving (Eq, Show, Ord)

data AssetCode = Stocks | Bonds 
      deriving (Eq, Show, Ord)

data Transfer = Currency CurrencySymbol | Asset AssetCode | Null
       deriving (Eq, Show, Ord)

data Date = Date  Int
      deriving (Show)

data Contract = Zero | 
                One Transfer | 
                Time (Obs Bool) Contract | 
                Scale (Obs Double) Contract | 
                Get Contract |
                Give Contract |
                And Contract Contract
      deriving (Show)

newtype Obs a = Obs (Date -> a)

instance Show a => Show (Obs a) where
   show (Obs o) = "(Obs " ++ show  (o today) ++ ")"


today :: Date
today = Date 0 

zero :: Contract
zero = Zero 

time :: Obs Bool -> Contract -> Contract
time = Time

one :: Transfer -> Contract
one = One 

scale :: Obs Double -> Contract -> Contract
scale = Scale 

get :: Contract -> Contract
get = Get

give :: Contract -> Contract
give = Give

and' :: Contract -> Contract -> Contract
and' = And 


-----Observables-----

--Constant to scale contract e.g 
konst :: a -> Obs a
konst k = Obs (\t -> k)

--Checks if time horizon has been reached
sameDate :: Date -> Date -> Bool
sameDate (Date t1) (Date t2) = (t1 == t2) 

--Checks current date and future date
at :: Date -> Obs Bool 
at t_future = Obs (\t ->  sameDate t_future t)

payout :: Double -> Double -> Double
payout o b = o * b



