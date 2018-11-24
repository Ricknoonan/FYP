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
today = Date 10

data Contract = Zero | 
                One Transfer | 
                Time (Obs Bool) Date Contract | 
                Scale Double Contract | 
                Get Contract |
                Give Contract |
                And Contract Contract       
        deriving (Show)

data ReadableContract = ZeroReadable |
                        OneReadable Double |
                        TimeReadable ReadableContract ReadableContract ReadableContract | 
                        ScaleReadable Double |
                        Amount Double | 
                        ExpireDate Date |
                        DateReached (Obs Bool) |
                        Join ReadableContract ReadableContract           
        deriving (Show)

newtype Obs a = Obs (Date -> a)

instance Show a => Show (Obs a) where
   show (Obs o) = show  (o today) 

--amountBet take the bet size and currency of bet and returns AmountBet of type ReadableContract
amount :: Double -> ReadableContract 
amount bet = Amount (bet)

dateReached :: Date -> ReadableContract
dateReached settleDate = 
    DateReached (at settleDate)

expireDate :: Date -> ReadableContract
expireDate settleDate = 
    ExpireDate settleDate

oneReadable :: Contract -> Double
oneReadable (One transfer) = 1
 

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