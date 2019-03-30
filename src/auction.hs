module Auction where

import Contract
import ContractClass
import Prelude hiding (until)

endAuction :: Contract 
endAuction = 
    (function "endAuction" (when (TimesUp) (send (ToBeneficiary (Variable "highestBid")) (unless (AlreadyFinished) End))))

placeBid :: Contract 
placeBid = 
    (function "placeBid" (until (TimesUp) 
        (commitEther (Higher "highestBid") (addTo "bid" endAuction))))

takeCashOut :: Contract 
takeCashOut = 
    (function "withdraw" (send (Address All) (from "bid" placeBid)))
    
auction :: Contract
auction = 
    (constructor (set (TimeLimit) (set (Beneficiary) takeCashOut)))



