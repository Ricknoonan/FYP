module Contract where 

import Prelude hiding (and,or)
import Numeric
import Data.Unique
import Data.Time
import Data.Map (Map, (!))
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)

type Ether = Double 

type Address = String

type Decision = Address 

type OP = [Output]

data ContractState = ContractState {
                commits :: Map.Map Int Action,
                withdrawls :: Map.Map Int Action, 
                etherBalance  :: Ether,
                owner :: Address
             }
             deriving (Eq,Show,Ord)

data ParamState = ParamState {
                maxPeople :: Int ,
                amountSize :: Ether, 
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
                CommitEther InputCondition Contract| -- allows a person to commit x amount that is defined the in the contract
                CashBackAll Contract |
                Send SendCondition Contract |
                Withdraw Contract |
                Allow Modifier Contract |
                Function String Contract |
                IsNot Contract |
                Set Parameter Contract |
                Constructor Contract |
                AddTo String Contract |
                From String Contract |
                Unless FunctionCondition Contract |
                Error String
        deriving (Show, Eq, Read)

data InputCondition = Min Ether |
                      Max Ether |
                      Equal Ether |
                      Higher String | 
                      Lower String |
                      NoLimit
            deriving (Show, Eq,Read)

data SendCondition = Winner PayOption |
                     Random PayOption |
                     ToOwner PayOption |
                     ToBeneficiary PayOption |
                     Highest PayOption |
                     Address PayOption
                deriving (Show, Eq,Read)

data FunctionCondition = AlreadyJoined |
                         AlreadyFinished
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
              Message String 
        deriving(Show,Read)

data Parameter =   Days Int |
                   Amount Ether|
                   TotalReached |
                   TimesUp |
                   People Int |
                   TotalAmount |
                   TimeLimit |
                   ContractOwner |
                   Beneficiary
                deriving (Show, Eq, Ord, Read)

data PayOption = All | 
                 Rest |
                 Partial Double |
                 Variable String 
                deriving (Show, Eq,Read)

data Action =  Commit Address Ether |
               SendOut Address Ether 
    deriving (Eq,Show,Ord,Read)

data Input = CashInp Address Ether |
             Decision Int |
             SetOwner Address |
             WithdrawEther Input Parameter |
             Empty
        deriving (Show, Eq, Ord,Read)

evalC :: Input -> Contract -> ParamState -> OP -> ContractState -> (Contract, OP, ContractState, ParamState)
evalC i@(CashInp address money) c@(CommitEther inpCon c1) pst o st 
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

evalC i c@(When param c1) pst o st
    | evalParam c pst st i = (c1, [], st, pst)
    | otherwise = (c, [], st, pst)

evalC i c@(Send param c1) pst o st 
    | length payees > 0 = (c1, (loopPayees payees payAmount), updateState, pst)
    | otherwise = (c, [SendFail], updateState, pst)
        where
            payAmount = (etherBalance st)/(payeesLength payees)
            newBal = evalValue (-) st payAmount 
            updateState = st {etherBalance = newBal,  withdrawls = Map.insert (withdrawlsSize st) (SendOut (flattenAddress payees) payAmount) wDraw}
            payees = evalSend param st i 
            wDraw = withdrawls st

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

evalC i c@(Unless funCon c1) pst o st = (c1, [], st, pst)


run :: Contract -> Input -> ParamState -> ContractState -> (Contract, OP, ContractState, ParamState)
run c@(CommitEther val c1) inp@(CashInp address money) pst s = evalC inp c pst [] s
run c@(Send address c1) inp@(Decision d) pst s = evalC inp c pst [] s
run c@(Withdraw c1) inp@(Decision d) pst s = evalC inp c pst [] s
run c inp pst s = evalC inp c pst [] s

evalInput :: InputCondition -> Ether -> Bool
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
    | ((commitAtIndex wal const) /= 0) = (commitAtIndex wal const >= amnt)
    | otherwise = True

--TODO Handle address that did not make a committment i.e Beneficiary
evalSend :: SendCondition -> ContractState -> Input -> [Address]
evalSend (Highest p) st (Decision d) = getAddress (findAtIndex (highestInMap (commits st)) st)
evalSend (Winner All) st (Decision d) = getAddress (findAtIndex([d]) st)
evalSend (Random All) st (Empty) = getAddress (findAtIndex([(sizeCommits st)]) st)
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
flattenAddress :: [Address] -> Address
flattenAddress [a] = a 

withdrawlsSize :: ContractState -> Int
withdrawlsSize s = ((Map.size (withdrawls s)) + 1)

cashInSize :: ContractState -> Int
cashInSize s = ((Map.size (commits s)) + 1)

sizeCommits :: ContractState -> Int
sizeCommits s = ((Map.size (commits s)))

index :: Int -> ContractState -> Int 
index p s
    | Map.member p (commits s) = index (p) s
    | otherwise = p 

commitSize :: ContractState -> Int 
commitSize s = Map.size (commits s)

withdrawSize :: ContractState -> Int 
withdrawSize s = Map.size (withdrawls s)

getMoneyCommit :: [Action] -> Ether 
getMoneyCommit [(Commit p m)] = m

getOneAddress :: [Action] -> Address
getOneAddress [(Commit p m)] = p

getAddress :: [Action] -> [Address]
getAddress [] = []
getAddress (Commit p m:rest) = p:getAddress rest

commitAtIndex :: Int -> ContractState -> Ether
commitAtIndex ind s = getMoneyCommit (findAtIndex [ind] s)

findAtIndex :: [Int] -> ContractState -> [Action]
findAtIndex [] s = [(Commit "No Commit" 0)]
findAtIndex [i] s = [((commits s) ! i)]
findAtIndex (i:is) s = ((commits s) ! i):findAtIndex is s

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

evalValue :: (Ether -> Ether -> Ether) -> ContractState -> Ether -> Ether 
evalValue f s val = f (etherBalance s) val 

payeesLength :: [Address] -> Double
payeesLength payees = fromIntegral (length payees)