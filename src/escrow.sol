pragma solidity ^0.4.11;

// v0.0.2
// Author(s): Danny Ryan

// Contract in Alpha. For educational purposes only

// # Escrow Contract #
// Involves three 'actors' -- 'sender', 'recipient', 'arbitrator'
// Holds Ether from 'sender' to be transferred to 'recipient'.
// Ether in contract is transferred to 'recipient' when two of the three 'actors' `confirm`.
// Contract can be `void`ed by 'sender' after `block.timestamp` is past 'timestampExpired'
// Ether is transferred to 'sender' upon successful `void`.

contract Escrow {
    /*
        Escrow is initialized with references to three parties (addresses)
        as well as the contract expireation timestamp
        The amount to be held in escrow can be sent upon initialization or in any transaction after.
        'timestampExpired' must be in the future.
    */
    function Escrow(address _sender, address _recipient, address _arbitrator, uint _timestampExpired) payable {
        assert(_timestampExpired > now);

        actors.push(_sender);
        actors.push(_recipient);
        actors.push(_arbitrator);
        timestampExpired = _timestampExpired;
    }

    /*
       Any of the initially specified actors can call confirm().
       Once there are enough confirmations (2) confirm releases funds to recipient.
    */
    function confirm() only_actor {
        confirmations[msg.sender] = true;
        if (isConfirmed()) {
            // use call to forward gas in case complex function receives gas
            assert(recipient().call.value(this.balance)());
        }
    }

    /*
        Sender can void escrow agreement after expiration.
        Voiding sends all funds held in contract back to the sender.
    */
    function void() only_sender {
        assert(now > timestampExpired);

        // use call to forward gas in case complex function receives gas
        assert(sender().call.value(this.balance)());
    }

    /*
       Sender of funds in contract.
       Only party that can void and return funds
    */
    function sender() constant returns (address) {
        return actors[0];
    }

    /*
       Recipient of funds in contract.
       Receives funds after two confirms from distinct valid parties
    */
    function recipient() constant returns (address) {
        return actors[1];
    }

    /*
       Arbitrator of escrow contract
       Can act as 1 of the 3 required actors for `confirm`ing
    */
    function arbitrator() constant returns (address) {
        return actors[2];
    }

    /*
       Count number of confirms
       returns true if two or more
    */
    function isConfirmed() constant returns (bool) {
        uint confCount = 0;
        for (uint i = 0; i < actors.length; i++) {
            if (confirmations[actors[i]]) {
                confCount++;
            }
        }
        return (confCount >= 2);
    }

    /*
       returns true if address is either the sender, recipient, or arbitrator
    */
    function isActor(address addr) constant returns (bool) {
        for (uint i = 0; i < actors.length; i++) {
            if (actors[i] == addr) return true;
        }
        return false;
    }

    modifier only_actor { require(isActor(msg.sender)); _; }
    modifier only_sender { require(sender() == msg.sender); _; }

    address[] public actors;
    mapping (address => bool) public confirmations;
    uint public timestampExpired;
}

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