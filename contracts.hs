module Contract where 

import Numeric
import Control.Monad
import Data.Unique
import Data.Time

data Currency = EUR | USD | GBP
      deriving (Eq, Show, Ord)

data Date = Date  Int
      deriving (Show)

data Contract = Zero | 
                One Currency | 
                Time (Obs Bool) Contract | 
                Scale (Obs Double) Contract | 
                Odds (Obs Double) Contract
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

one :: Currency -> Contract
one = One 

scale :: Obs Double -> Contract -> Contract
scale = Scale 

odds :: Obs Double -> Contract -> Contract
odds = Odds 


-----Observables-----

--Constant to scale contract e.g 
konst :: a -> Obs a
konst k = Obs (\t -> k)

----Needs work----
--ToDo: look at paper again to see how time horizon is reached
--Checks if time horizon has been reached
sameDate :: Date -> Date -> Bool
sameDate (Date t1) (Date t2) = (t1 == t2) 

--Checks current date and future date
at :: Date -> Obs Bool 
at t_future = Obs (\t ->  sameDate t_future t)
----Needs Work-----
