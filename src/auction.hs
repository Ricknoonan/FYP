module Solidity where

import Contract
import ContractClass

auction contructor 

function to place bid 

fucntion to withdraw 


placeBid :: Contract 
placeBid = 
	(function "placeBid" (allow (NotOwner) (until (TimesUp) (cashIn (Higher) (addTo "bid")))))

withdraw :: Contract 
withdraw = 
	(function "withdraw" (allow (OnlyOwner) (when (TimesUp) (send (Owner All) placeBid))))

createAuction :: Contract
createAuction = 
	(contructor (set (TimeLimit) (set (ContractOwner) withdraw)))

