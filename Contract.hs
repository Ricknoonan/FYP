module Contract where 

import Prelude hiding (and)
import Numeric
import Control.Monad
import Data.Unique
import Data.Time.Calendar 
import Data.Time.LocalTime

data CurrencySymbol = EUR | USD | GBP
      deriving (Eq, Show, Ord)

data Transfer = Currency CurrencySymbol | Null
       deriving (Eq, Show, Ord)

data Date = Date  Int
      deriving (Show)

today :: Date
today = Date 0

data Contract = Zero |
                One Transfer |
                Time (Obs Bool) Contract | 
                Scale Double Contract |
                Give Contract | 
                And Contract Contract |
                Or Contract Contract     
        deriving (Show)

data ReadableContract = Empty String |
                        Single Transfer |
                        Payout Double |
                        Send ReadableContract |
                        Expired (Obs Bool) |
                        BetterContract ReadableContract  |
                        Join ReadableContract ReadableContract |
                        TimeReadable ReadableContract ReadableContract         
        deriving (Show)

newtype Obs a = Obs (Date -> a)

instance Show a => Show (Obs a) where
   show (Obs o) = show  (o today) 

--evalC takes the primitive contracts and performs the neccesary computations on
--and returns a double that can be used in evalR
evalC :: Contract -> Double 
evalC Zero           = 0
evalC (One t)        = 1
evalC (o `Scale` c)  = o * evalC c
evalC (o `Time` c)   = evalC c
evalC (c1 `And` c2)  = evalC c1 + evalC c2
evalC (c1 `Or` c2)   = max (evalC c1) (evalC c2)

--evalR takes the primitive contracts and returns readable contracts using evalC when needed
evalR :: Contract -> ReadableContract
evalR Zero          = Empty "Empty"
evalR (One t)       = Single t
evalR (o `Scale` c) = Payout (evalC (o `Scale` c))
evalR (Give c)      = Send (evalR c)
evalR (o `Time` c)  = TimeReadable (Expired o) (evalR c)
evalR (c1 `And` c2) = (evalR c1) `Join` (evalR c2)
evalR (c1 `Or` c2)  = if (evalC (c1 `Or` c2)) == (evalC c1)
                        then BetterContract (evalR c1)
                        else BetterContract (evalR c2)
                   
--Observables--

--Constant to scale contract 
konst :: a -> Obs a
konst k = Obs (\t -> k)

--Checks if time horizon has been reached
sameDate :: Date -> Date -> Bool
sameDate (Date t1) (Date t2) = (t1 == t2) 

--Checks current date and future date
at :: Date -> Obs Bool 
at t_future = Obs (\t ->  sameDate t_future t)