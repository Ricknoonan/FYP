module BettingContract where 

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

--Checks if time horizon has been reached
sameDate :: Date -> Date -> Bool
sameDate (Date t1) (Date t2) = (t1 == t2) 

at :: Date -> Obs Bool 
at t_future = Obs (\t ->  sameDate t_future t)

--This function returns type contract called time by combining a one contract, 
-- a scale contract and an odds contract. 
betCon :: Date -> Double -> Double -> Currency -> Contract
betCon t k o cur = time (at t) (odds (konst o) (scale (konst k) (one cur)))


bet1 = betCon (Date 5) 100 10 EUR 

