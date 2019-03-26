module Tests where

import Contract
import ContractClass

whenTest :: Either String Contract
whenTest = 
	(when (People 10) End)
