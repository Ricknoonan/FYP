module ToContract where

import Contract 
import ContractClass
import Simulate
import ToSolidity

readDataTypes :: [String] -> Contract
readDataTypes [] = End  
readDataTypes ("function":x:xs) = (Function x (readDataTypes xs))
readDataTypes ("constructor":xs) = (Constructor (readDataTypes xs))
readDataTypes ("set":x:xs) = 
	case x of 
		("contractowner") -> (Set (ContractOwner) (readDataTypes xs))
		("timelimit") -> (Set (ContractOwner) (readDataTypes xs))
		("totalamount") -> (Set (TotalAmount) (readDataTypes xs))
		_ -> (Error (x ++ ": Incorrect Parameter for Set"))
 
readDataTypes ("cashin":"nolimit":xs) = (CashIn (NoLimit) (readDataTypes xs))
readDataTypes ("addto":x:xs) = (AddTo x (readDataTypes xs))
readDataTypes ("withdraw": xs) = (Withdraw (readDataTypes xs)) 