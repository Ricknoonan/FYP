module Contract where 

import Prelude hiding (and,or)
import Numeric
import Data.Unique
import Data.Time
import Data.Map (Map, (!))
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)

type Person = Address  

type Money = Double 

type Owner = Person

type Address = String

type Decision = Person 

type OP = [Output]

data ContractState = ContractState {
                commits :: Map.Map Int Action,
                etherBalance  :: Money,
                owner :: Person
             }
             deriving (Eq,Show,Ord)

data ParamState = ParamState {
                maxPeople :: Int ,
                amountSize :: Money, 
                duration :: (Integer, Int, Int)
              }
              deriving (Show)

emptyCState :: ContractState
emptyCState = ContractState {commits = Map.empty, etherBalance = 0, owner = "0"}

emptyPState :: ParamState 
emptyPState = ParamState {maxPeople = 0, amountSize = 0, duration = (0,0,0)}

data Contract = End |
                Single Parameter |
                When Parameter Contract| -- When observable is true, next action can happen
                And Contract Contract|
                Or Contract Contract |
                Until Parameter Contract | -- until a certain observable, the following contracts can be evaluated
                CashIn InputCondition Contract| -- allows a person to commit x amount that is defined the in the contract
                CashBackAll Contract |
                Send SendCondition Contract | -- Sends person depening on event 
                Initiate Contract |
                Allow Parameter Contract |
                Function String Contract |
                Constructor Contract |
                Return String Contract 
        deriving (Show, Eq)

data InputCondition = Min Money |
                      Max Money |
                      Equal Money |
                      NoLimit
            deriving (Show, Eq)

data SendCondition = Winner PayOption |
                     Highest PayOption|
                     Random PayOption |
                     Owner PayOption |
                     Beneficiary PayOption |
                     Person PayOption
                deriving (Show, Eq)

data Output = Null |
              CommitFail Action |
              CommitPass Action |
              SendFail |
              SendSuccess Action |
              CashRedeemed [Action] |
              OwnerSet Address |
              Message String |
              ObNotReached 
        deriving(Show)

data Parameter =   Date (Integer, Int, Int) |
                   Amount Money |
                   People Int 
                deriving (Show, Eq, Ord)

data PayOption = All | 
                 Rest |
                 Withdraw |
                 Partial Money 
                deriving (Show, Eq, Ord)

data Action =  Commit Person Money |
               SendOut Person Money 
    deriving (Eq,Show,Ord)

data Input = CashInp Address Money |
             Decision Int |
             SetOwner Address |
             Empty
        deriving (Show, Eq, Ord)

-- Takes a function, the current balance and the amount being committed and returns new balance
evalValue :: (Money -> Money -> Money) -> ContractState -> Money -> Money 
evalValue f s val = f (etherBalance s) val 

payeesLength :: [Address] -> Double
payeesLength payees = fromIntegral (length payees) 

-- TODO: Should move to next contract when contract is full not accept another input, fail and then move on
evalC :: Input -> Contract -> ParamState -> OP -> ContractState -> (Contract, OP, ContractState, ParamState)
evalC i@(CashInp address money) c@(CashIn inpCon c1) pst o st 
    | evalInput inpCon money && evalParam c pst st i && ((size st + 1) < maxPeople pst) = (c, outputP, updateState, pst) 
    | (size st + 1) == (maxPeople pst) = (c1, outputP, updateState, pst)
    | not (evalParam c pst st i) || (size st == maxPeople pst) = (c1, outputF, st, pst)
    | otherwise = (c, outputF, st, pst)
        where
            newBal = evalValue (+) st money 
            comms = commits st
            outputP = [CommitPass (Commit address money)]
            outputF = [CommitFail (Commit address money)]
            updateState = st {commits = Map.insert (index (maxPeople pst) st) (Commit address money) comms, etherBalance = newBal}

-- Infinite loop on fail 
evalC i c@(When param c1) pst o st
    | evalParam c pst st i = (c1, [], st, pst)
    | otherwise = (c, [], st, pst)

-- Needs to check if contract horizon is reached 
evalC i c@(Send param c1) pst o st 
    | length payees > 0 = (End, (loopPayees payees payAmount), updateState, pst)
    | otherwise = (c, [SendFail], updateState, pst)
        where
            payAmount = (etherBalance st)/(payeesLength payees)
            newBal = evalValue (-) st payAmount 
            updateState = st {etherBalance = newBal}
            payees = evalSend param st i 

evalC i c@(CashBackAll c1) pst o st 
    | evalParam c pst st i = (c1, o, updateState, pst)
    | (size st) > 0 = evalC i c pst (o ++ [no]) updateState
    | otherwise = evalC i c1 pst o st
        where 
            no = CashRedeemed (findAtIndex ([size st]) st)
            newCommits = Map.delete (size st) (commits st)
            currentBal = etherBalance st
            refund = commitAtIndex (size st) st
            updateState = st {commits = newCommits, etherBalance = (currentBal - refund)}

evalC i c@(Until param c1) pst o st =
    case param of 
        (Date (y,m,d)) -> (c1, [], st, pst {duration = (y,m,d)})
        (Amount x) -> (c1, [], st, pst {amountSize = x})
        (People p) -> (c1, [], st, pst {maxPeople = p})

evalC i@(SetOwner address) c@(Initiate c1) pst o st = 
    (c1, [OwnerSet address], st {owner = address} ,pst)

evalC i c@(Function str c1) pst o st = evalC i c1 pst o st
{--
payAmount :: Input -> ContractState -> [Address] -> SendCondition -> Money
payAmount i st p (Winner) = etherBalance st/p 
payAmount i st p (Highest) = etherBalance st/p
payAmount (Decision d) st p (Withdraw x) 
    | 
--}
  --check persons balance see if they have deposited that amount, asssuming yes take that amount from 
    -- that persons account and update balance

run :: Contract -> Input -> ParamState -> ContractState -> (Contract, OP, ContractState, ParamState)
run c@(CashIn val c1) inp@(CashInp address money) pst s = evalC inp c pst [] s
run c@(Send address c1) inp@(Decision d) pst s = evalC inp c pst [] s
run c inp pst s = evalC inp c pst [] s

evalInput :: InputCondition -> Money -> Bool
evalInput (Min m) inp = (inp > m )
evalInput (Max m) inp = (inp < m )
evalInput (Equal m) inp = (inp == m)
evalInput (NoLimit) inp = True

evalParam :: Contract -> ParamState -> ContractState -> Input -> Bool
evalParam c pst const (CashInp address money)
    | (duration pst) /= (0,0,0) = at (Date (duration pst))
    | (amountSize pst) /= 0 = (etherBalance const + money) <= amountSize pst 
    | otherwise = True

evalParam (When para c1) pst const i = 
    case para of 
        (Date (y,m,d)) -> at (Date (y,m,d))
        (Amount x) -> (etherBalance const) == x 
        _ -> False

evalParam (CashBackAll c1) pst const i
    | (duration pst) /= (0,0,0) = at (Date (duration pst))
    | otherwise = True

--TODO Handle address that did not make a committment i.e Beneficiary
evalSend :: SendCondition -> ContractState -> Input -> [Address]
evalSend (Highest p) st (Decision d) = getAddress (findAtIndex (highestInMap (commits st)) st)
evalSend (Winner p) st (Decision d) = getAddress (findAtIndex([d]) st)

sameDate :: Parameter -> Parameter -> Bool
sameDate (Date (t1, t2, t3)) (Date (t4, t5, t6)) 
    | (t1 > t4 ) = False 
    | (t1 > t4 ) && (t2 > t5) = False
    | (t1 > t4 ) && (t2 > t5) && (t3 > t6) = False
    | otherwise = True

at :: Parameter -> Bool 
at tContract = sameDate tContract (Date (unsafePerformIO (todayDate)))

todayDate :: IO (Integer,Int,Int) -- :: (year,month,day)
todayDate = getCurrentTime >>= return . toGregorian . utctDay

index :: Int -> ContractState -> Int 
index p s
    | Map.member p (commits s) = index (p-1) s
    | otherwise = p 

size :: ContractState -> Int 
size s = Map.size (commits s)

getMoneyCommit :: [Action] -> Money 
getMoneyCommit [(Commit p m)] = m

getAddress :: [Action] -> [Address]
getAddress [(Commit p m)] = [p]
getAddress (Commit p m:rest) = p:getAddress rest

commitAtIndex :: Int -> ContractState -> Money
commitAtIndex ind s = getMoneyCommit (findAtIndex [ind] s)

findAtIndex :: [Int] -> ContractState -> [Action]
findAtIndex [] s = [(Commit "No Commit" 0)]
findAtIndex [i] s = [(Map.findWithDefault (Commit "No Commit" 0) (i) (commits s))]
findAtIndex (i:is) s = (Map.findWithDefault (Commit "No Commit" 0) (i) (commits s)):findAtIndex is s

highestInMap :: Map Int Action -> [Int]
highestInMap m = go [] Nothing (Map.toList m)
  where
    go ks _        []           = ks 
    go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | getMoneyCommit [v] < getMoneyCommit [u] = go ks (Just u) rest
        | getMoneyCommit [v] > getMoneyCommit [u] = go [k] (Just v) rest
        | otherwise = go (k:ks) (Just v) rest

loopPayees :: [String] -> Double -> OP
loopPayees [x] payAmount = [SendSuccess (SendOut x payAmount)]
loopPayees (x:xs) payAmount = SendSuccess (SendOut x payAmount) : loopPayees xs payAmount
