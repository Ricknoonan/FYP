module Contract where 

import Prelude hiding (and)
import Numeric
import Control.Monad
import Data.Unique
import Data.Time.Calendar 
import Data.Time.LocalTime

data CurrencySymbol = EUR | USD | GBP
      deriving (Eq, Show, Ord)

currency :: Transfer -> String
currency (Currency EUR) = (show (EUR))
currency (Currency USD) = (show (USD))
currency (Currency GBP) = (show (GBP))

data Transfer = Currency CurrencySymbol | Null
       deriving (Eq, Show, Ord)

data Date = Date  Int
      deriving (Show)

today :: Date
today = Date 0

data Contract = Zero | 
                One Transfer | 
                Time (Obs Bool) Date Contract | 
                Scale Double Contract | 
                Get Contract |
                Give Contract |
                And Contract Contract       
        deriving (Show)

data ReadableContract = Empty |
                        Single String Double |
                        Amount String Double |
                        Payout String Double |
                        AtContractExpire ReadableContract ReadableContract ReadableContract|
                        ExpireDate String |
                        DateReached (Obs Bool) |
                        FinalContract ReadableContract Double ReadableContract  
        deriving (Show)

newtype Obs a = Obs (Date -> a)

instance Show a => Show (Obs a) where
   show (Obs o) = show  (o today) 

--amountBet take the bet size and currency of bet and returns AmountBet of type ReadableContract
amount :: Double -> Transfer -> ReadableContract 
amount bet cur = 
    Amount (currency (cur)) (bet)

dateReached :: Date -> ReadableContract
dateReached settleDate = 
    DateReached (at settleDate)

expireDate :: Date -> ReadableContract
expireDate settleDate = 
    ExpireDate (show settleDate)

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