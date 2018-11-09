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
                Scale Double Contract | 
                Get Contract |
                Give Contract |
                And Contract Contract |
                Bet Contract
      deriving (Show)

-- \item Amount bet {Currency}{Amount}. Odds {Odds}. Payout {Currency}{Amount*Odds} on {Date}
-- \item Amount bet €100. Odds 10/1. Payout €1000

data ReadableContract = AmountBet String |
                        Odds String |
                        Payout String |
                        BettingContract ReadableContract ReadableContract ReadableContract
      deriving (Show)

newtype Obs a = Obs (Date -> a)

instance Show a => Show (Obs a) where
   show (Obs o) = "(Obs " ++ show  (o today) ++ ")"


-------ReadableContract--------
--This takes the One contract and returns a double 1 for the purpose of scaling 
oneInteger :: Contract -> Double
oneInteger (One transfer) = 1

--payout takes a scale contract and returns Payout of type ReadableContract by multiplying the odds by the bet and converting to String using show
payout :: Contract -> ReadableContract
payout (Scale odds (Scale bet (One cur))) = 
    Payout ((currency cur)++(show(odds * bet * oneInteger (One cur))))

--amountBet take the bet size and currency of bet and returns AmountBet of type ReadableContract
amountBet :: Double -> Transfer -> ReadableContract 
amountBet bet cur = 
    AmountBet (currency (cur)++show bet)

--atOdds convets odds to Strig
atOdds :: Double -> ReadableContract
atOdds odds = 
    Odds (show odds)

--bettingContract take the 
bettingContract :: Double -> Double -> Transfer -> ReadableContract
bettingContract odds bet currency = 
    BettingContract (amountBet bet currency) (atOdds odds) (payout (Scale odds (Scale bet (One currency))))

bettingContract1 = bettingContract 10 10 (Currency EUR)

currency :: Transfer -> String
currency (Currency EUR) = (show (EUR))
currency (Currency USD) = (show (USD))
currency (Currency GBP) = (show (GBP))
--------End ReadableContract--------------


today :: Date
today = Date 0 

zero :: Contract
zero = Zero 

time :: Obs Bool -> Contract -> Contract
time = Time

one :: Transfer -> Contract
one = One 

scale :: Double -> Contract -> Contract
scale = Scale 

get :: Contract -> Contract
get = Get

give :: Contract -> Contract
give = Give

and' :: Contract -> Contract -> Contract
and' = And 

bet :: Contract -> Contract
bet = Bet

-----Observables-----

--Constant to scale contract 
konst :: a -> Obs a
konst k = Obs (\t -> k)

--Checks if time horizon has been reached
sameDate :: Date -> Date -> Bool
sameDate (Date t1) (Date t2) = (t1 == t2) 

--Checks current date and future date
at :: Date -> Obs Bool 
at t_future = Obs (\t ->  sameDate t_future t)




