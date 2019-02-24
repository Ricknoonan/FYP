module Betting where

import Contract
import ContractClass
import Prelude hiding (until)

--Betting contract, two people enter a betting contract, when someone/something decides who one money
--is paid to winner
{--
crowdfundingContract :: Contract 
crowdfundingContract = 
	(until (Ob (Date (2018,12,13)))
        (cashInUnlimited address (people 3)
            (when (Ob(Amount 50))
                (pay (Ob (Winner address)) End)                         	
                (cashBackAll End))
        End)
    End)
              
              escrow :: Contract 
escrow = 
    (until (Ob 
           (Date (2019,04,1)))
           (and 
                (cashIn (Ob 
                        (People 1))
                (join (Ob 
                      (People 2)))
            (when (Ob 
                  (Date (2019,05,01)))
                (send 
                    (Ob (Beneficiary))
                End)
            End)
        End)
    End)
--}
bettingContract :: Contract
bettingContract =  initiateOwnerShip 5 2 (2019,03,01)  

simpleBet :: (Integer, Int, Int) -> Contract
simpleBet d =
	(until (Date d) (cashIn (Max 5) (send (Winner All) End)))

limitPeople :: Int -> (Integer, Int, Int) -> Contract 
limitPeople p d = 
	(until (People p) (simpleBet d))

limitAmount :: Money -> Int -> (Integer, Int, Int) -> Contract
limitAmount m p d = 
	(until (Amount m) (limitPeople p d))
	
initiateOwnerShip :: Money -> Int -> (Integer, Int, Int) -> Contract
initiateOwnerShip m p d = 
	(initiate (limitAmount m p d))


-- at this date you can allow people to enter your
	                                     
{--
function openBetSession(uint betAmount, uint duration, uint people) external onlyOwner {
        require(duration > 0);
        require(betAmount > 5);

        // 1. Saves the timestamp the bet was opened.
        // 2. Do not allow concurrent betting sessions, by setting sessions as ongoing.
        // 3. Creates a new betting session using the specified parameters.
        // 4. Sets a new unique identifier for the bet session.
        // 5. Raises an event notifying a new betting session was open.
        uint openedAt = block.timestamp;
        ongoingSession = true;
        sessions.push(BetSession(minAmount, fee, duration, openedAt, 0, 0, 0, 0, 0));
        sessionIndex = sessions.length - 1;
        emit BetSessionOpened(sessionIndex, minAmount, duration, openedAt);
    }

function placeBet(uint option) external payable notOwner openForBets {
        // Player's bet value must meet minimum bet requirement.
        // Player's option must be a valid bet option. Value must be in (0==heads; 1==tails).
        require(msg.value >= sessions[sessionIndex].minimumBet);
        require(option <= uint(BetOption.TAIL));

        // 1. Creates a new Bet and assigns it to the list of bets.
        // 2. Updates current betting session stats.
        // 3. Raises an event for the bet placed by the player.
        //betsBySession[sessionIndex].push(Bet(player, msg.value, BetOption(option)));  // See note at beginning of function.
        betsBySession[sessionIndex].push(Bet(msg.sender, msg.value, BetOption(option)));
        updateSessionStats(BetOption(option), msg.value);
        emit NewBetPlaced(sessionIndex, msg.sender, msg.value, BetOption(option));
    }

function rewardWinners(BetOption result) private onlyOwner closedForBets {
        // 1. Calculates the fee that goes to the house/contract.
        // 2. Calculates the total prize that can be paid out to winners, after paying the owner/house.
        // 3. Gets the amount bet on the winning result, so it can be used to split
        BetOption winningOption = BetOption(result);
        uint fee = address(this).balance * sessions[sessionIndex].ownerFee / 100;
        uint totalPrize = address(this).balance - fee;
        uint winningBetAmount;
        if (winningOption == BetOption.HEAD) {
            winningBetAmount = sessions[sessionIndex].headsAmount;
        } else {
            winningBetAmount = sessions[sessionIndex].tailsAmount;
        }

        // 4. Pays out players.
        // Calculates the ratio between player's bet amount and the total prize,
        // to determine the player's prize.
        for (uint i = 0; i < betsBySession[sessionIndex].length; i++) {
            Bet memory curBet = betsBySession[sessionIndex][i];
            if (curBet.option == winningOption) {
                // Gets the percentage/ratio of the player's bet,
                // em relation to the amount betted on the winning result.
                uint relativeBetSize = curBet.amount / winningBetAmount * 100;
                // Calculates the prize for the player, considering its 
                // stake (relativeBetSize) em relation to the total prize.
                uint prize = totalPrize * relativeBetSize / 100;
                // Pays the player.
                curBet.player.transfer(prize);
            }
            // No prize for losers.
        }

        --}

        {--
function betAndFlip() public               
    {
    	if(msg.value > 340282366920938463463374607431768211455)  	// value can't be larger than (2^128 - 1) which is the uint128 limit
    	{
    		lastresult = "wager too large";
    		lastgainloss = 0;
    		msg.sender.send(msg.value); // return wager
    		return;
    	}		  
    	else if((msg.value * 2) > this.balance) 					// contract has to have 2*wager funds to be able to pay out. (current balance INCLUDES the wager sent)
    	{
    		lastresult = "wager larger than contract's ability to pay";
    		lastgainloss = 0;
    		msg.sender.send(msg.value); // return wager
    		return;
    	}
    	else if (msg.value == 0)
    	{
    		lastresult = "wager was zero";
    		lastgainloss = 0;
    		// nothing wagered, nothing returned
    		return;
    	}


Contract Bet {

int people; 
string output;
int winningNumber;

 function Bet() private 
    {                              
        people = 0;
        output = "No bets"
        winningNumber = 2; //obviously this isn't correct, just for demo purposes
    }

function joinBet (uint8 guess) public 
    {
        if (people >= 2){
            msg.sender.send(msg.value); //return money to sender
        }
        if (msg.value /= 5){
            msg.sender.send(msg.value);
            output = "Bet size is 5"
        }
        else{
        people++; 
        }
        if (people == 2){
            if(guess == winningNumber){
                msg.sender.send(msg.value*2) //sender double their amount i.e the contract balance
            }
            else 
                return;
        }
    }
}

--}