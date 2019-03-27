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
                withdrawls :: Map.Map Int Action, 
                etherBalance  :: Money,
                owner :: Person
             }
             deriving (Eq,Show,Ord)

data ParamState = ParamState {
                maxPeople :: Int ,
                amountSize :: Money, 
                duration :: Int
              }
              deriving (Show)

emptyCState :: ContractState
emptyCState = ContractState {commits = Map.empty, withdrawls = Map.empty, etherBalance = 0, owner = "No Owner"}

emptyPState :: ParamState 
emptyPState = ParamState {maxPeople = 0, amountSize = 0, duration = 0}

data Contract = End |
                When Parameter Contract| -- When observable is true, next action can happen
                And Contract Contract|
                Or Contract Contract |
                Until Parameter Contract | -- until a certain observable, the following contracts can be evaluated
                CashIn InputCondition Contract| -- allows a person to commit x amount that is defined the in the contract
                CashBackAll Contract |
                Send SendCondition Contract |
                Withdraw Contract |
                Allow Modifier Contract |
                Function String Contract |
                Not Contract |
                Set Parameter Contract |
                Constructor Contract |
                AddTo String Contract |
                Unless FunctionCondition Contract |
                Error String
        deriving (Show, Eq, Read)

data InputCondition = Min Money |
                      Max Money |
                      Equal Money |
                      Higher String | 
                      Lower |
                      NoLimit
            deriving (Show, Eq,Read)

data SendCondition = Winner PayOption |
                     Highest PayOption|
                     Random PayOption |
                     ToOwner PayOption |
                     Beneficiary PayOption |
                     Person PayOption
                deriving (Show, Eq,Read)

data FunctionCondition = AlreadyJoined 
             deriving (Show, Eq,Read)

data Modifier = OnlyOwner |
                NotOwner 
            deriving (Show, Eq, Read)

data Output = Null |
              CommitFail Action |
              CommitPass Action |
              SendFail |
              SendSuccess Action |
              OwnerSet Address |
              WithdrawPass [Action] |
              WithdrawFail [Action] |
              Message String |
              ObNotReached 
        deriving(Show,Read)

data Parameter =   Days Int |
                   Amount Money|
                   TotalReached |
                   TimesUp |
                   People Int |
                   TotalAmount |
                   TimeLimit Int |
                   ContractOwner 
                deriving (Show, Eq, Ord, Read)

data PayOption = All | 
                 Rest |
                 Partial Money 
                deriving (Show, Eq, Ord,Read)

data Action =  Commit Person Money |
               SendOut Person Money 
    deriving (Eq,Show,Ord,Read)

data Input = CashInp Address Money |
             Decision Int |
             SetOwner Address |
             WithdrawEther Input Parameter |
             Empty
        deriving (Show, Eq, Ord,Read)

-- Takes a function, the current balance and the amount being committed and returns new balance
evalValue :: (Money -> Money -> Money) -> ContractState -> Money -> Money 
evalValue f s val = f (etherBalance s) val 

payeesLength :: [Address] -> Double
payeesLength payees = fromIntegral (length payees) 






-- TODO: Should move to next contract when contract is full not accept another input, fail and then move on
evalC :: Input -> Contract -> ParamState -> OP -> ContractState -> (Contract, OP, ContractState, ParamState)
evalC i@(CashInp address money) c@(CashIn inpCon c1) pst o st 
    | evalInput inpCon money && evalParam c pst st i && ((maxPeople pst) /= 0)= (c, outputP, updateState, pst)
    | evalInput inpCon money && evalParam c pst st i && ((maxPeople pst) == 0) = (c1, outputP, updateState, pst)
    | (commitSize st + 1) == (maxPeople pst) = (c1, outputP, updateState, pst)
    | not (evalParam c pst st i) || (commitSize st == maxPeople pst) = (c1, outputF, st, pst)
    | otherwise = (c, outputF, st, pst)
        where
            newBal = evalValue (+) st money 
            comms = commits st
            outputP = [CommitPass (Commit address money)]
            outputF = [CommitFail (Commit address money)]
            updateState = st {commits = Map.insert (cashInSize st) (Commit address money) comms, etherBalance = newBal}

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
    | (commitSize st) > 0 = evalC i c pst (o ++ [no]) updateState
    | otherwise = evalC i c1 pst o st
        where 
            no = WithdrawPass (findAtIndex ([commitSize st]) st)
            newCommits = Map.delete (commitSize st) (commits st)
            currentBal = etherBalance st
            refund = commitAtIndex (commitSize st) st
            updateState = st {commits = newCommits, etherBalance = (currentBal - refund)}

evalC i c@(Until param c1) pst o st =
    case param of 
        (Days d) -> (c1, [], st, pst {duration = d})
        (Amount x) -> (c1, [], st, pst {amountSize = x})
        (People p) -> (c1, [], st, pst {maxPeople = p})

evalC i@(SetOwner address) c@(Set (ContractOwner) c1) pst o st = 
    (c1, [OwnerSet address], st {owner = address} ,pst)

evalC i@(WithdrawEther (Decision wal) (Amount amnt)) c@(Withdraw c1) pst o st 
    | evalParam c pst st i && (commitSize st) > 0  = (c1, outputP, updateState, pst)
    | otherwise = (c, outputF, st, pst)
        where 
          wDraw = withdrawls st
          address = getOneAddress (findAtIndex [wal] st )
          outputP = [WithdrawPass [SendOut address amnt]] 
          outputF = [WithdrawFail [SendOut address amnt]]
          currentBal = etherBalance st
          updateState = st {etherBalance = currentBal - amnt, withdrawls = Map.insert (withdrawlsSize st) (SendOut address amnt) wDraw}

evalC i c@(Function str c1) pst o st = (c1, [outputFun], st, pst) 
    where 
      outputFun = (Message ("Function " ++ str ++ " created"))
 
evalC i c@(Constructor c1) pst o st = (c1, [outputCon], st, pst)
    where 
      outputCon = (Message ("Constructor created" ))

evalC i c@(Set param c1) pst o st = (c1, [], st, pst)
    where 
      outputSet = (Message (""))

evalC i c@(AddTo str c1) pst o st = (c1, [], st, pst)

evalC i c@(End) pst o st = (End, [], st, pst)








run :: Contract -> Input -> ParamState -> ContractState -> (Contract, OP, ContractState, ParamState)
run c@(CashIn val c1) inp@(CashInp address money) pst s = evalC inp c pst [] s
run c@(Send address c1) inp@(Decision d) pst s = evalC inp c pst [] s
run c@(Withdraw c1) inp@(Decision d) pst s = evalC inp c pst [] s
run c inp pst s = evalC inp c pst [] s

evalInput :: InputCondition -> Money -> Bool
evalInput (Min m) inp = (inp > m )
evalInput (Max m) inp = (inp < m )
evalInput (Equal m) inp = (inp == m)
evalInput (NoLimit) inp = True

evalParam :: Contract -> ParamState -> ContractState -> Input -> Bool
evalParam c pst const (CashInp address money)
    | (duration pst) /= 0 = at (Days (duration pst))
    | (amountSize pst) /= 0 = (etherBalance const + money) <= amountSize pst 
    | (maxPeople pst) /= 0 = ((commitSize const + 1) < maxPeople pst)
    | otherwise = True

evalParam (Send param c1) pst const (Decision dec)
    | (duration pst) /= 0 = at (Days (duration pst))
    | otherwise = True 

evalParam (When para c1) pst const i = 
    case para of 
        (Days d) -> at (Days d)
        (Amount x) -> (etherBalance const) == x 
        _ -> False

evalParam (CashBackAll c1) pst const i
    | (duration pst) /= 0 = at (Days (duration pst))
    | otherwise = True

evalParam (Withdraw c1) pst const (WithdrawEther (Decision wal) (Amount amnt))
    | ((commitAtIndex wal const) /= 0) = (commitAtIndex wal const > amnt)
    | otherwise = True

--TODO Handle address that did not make a committment i.e Beneficiary
evalSend :: SendCondition -> ContractState -> Input -> [Address]
evalSend (Highest p) st (Decision d) = getAddress (findAtIndex (highestInMap (commits st)) st)
evalSend (Winner All) st (Decision d) = getAddress (findAtIndex([d]) st)
evalSend _ st _ = ["No Address"]

--TODO this needs to be changed to unix timestamps instead of hard dates
---------------
sameDate :: Parameter -> Parameter -> Bool
sameDate (Days inContract) (Days now) 
    | now < inContract = False
    | now == inContract = True
    | otherwise = False

at :: Parameter -> Bool 
at tContract = sameDate tContract (intialDays)

intialDays :: Parameter 
intialDays = Days 0

--------------

updateBalance:: Int -> [Action] -> Money -> ContractState -> Maybe Action
updateBalance i a m const = 
  if (findAtIndex [i] const) == a then Just (Commit (getOneAddress a) m) else Nothing

{--
O(log n). The expression (update f k map) updates the value x at k (if it is in the map). If (f x) is Nothing, the element is deleted. If it is (Just y), the key k is bound to the new value y.

 let f x = if x == "a" then Just "new a" else Nothing
 update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
 update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
 update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
--}

withdrawlsSize :: ContractState -> Int
withdrawlsSize s = ((Map.size (withdrawls s)) + 1)

cashInSize :: ContractState -> Int
cashInSize s = ((Map.size (commits s)) + 1)

index :: Int -> ContractState -> Int 
index p s
    | Map.member p (commits s) = index (p) s
    | otherwise = p 

commitSize :: ContractState -> Int 
commitSize s = Map.size (commits s)

withdrawSize :: ContractState -> Int 
withdrawSize s = Map.size (withdrawls s)

getMoneyCommit :: [Action] -> Money 
getMoneyCommit [(Commit p m)] = m

getOneAddress :: [Action] -> Address
getOneAddress [(Commit p m)] = p

getAddress :: [Action] -> [Address]
getAddress [] = []
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
