module Auction where

import Contract
import ContractClass

placeBid :: Contract 
placeBid = 
	(function "placeBid" (allow (NotOwner) (until (TimesUp) (cashIn (Higher "highestBid") (addTo "bid")))))

withdraw :: Contract 
withdraw = 
	(function "withdraw" (allow (OnlyOwner) (when (TimesUp) (send (Owner All) placeBid))))

createAuction :: Contract
createAuction = 
	(contructor (set (TimeLimit) (set (ContractOwner) withdraw)))

