module Auction where

import Contract
import ContractClass
import Prelude hiding (until)

auction :: Contract
auction = 
    (constructor (set (TimeLimit) (set (Beneficiary) takeCashOut)))

takeCashOut :: Contract 
takeCashOut = 
    (function "withdraw" (send (Address All) (from "bid" placeBid)))

placeBid :: Contract 
placeBid = 
    (function "placeBid" (until (TimesUp) 
        (commitEther (HigherThan "highestBid") (addTo "bid" endAuction))))

endAuction :: Contract 
endAuction = 
    (function "endAuction" (when (TimesUp) (send (ToBeneficiary (AmountIn "highestBid")) (unless (AlreadyFinished) End))))



