pragma solidity ^0.5.0;
contract Contract {
address owner;
mapping (address => uint) private "balance";


constructor()public {
owner = msg.sender;

}
function "deposit"()public payable returns (uint){
"balance"[msg.sender] += msg.value;
return "balance"[msg.sender];

}
function "cashout"()public returns (unit remainingBal){
if (withdrawAmount <="balance"[msg.sender]) {
"balance"[msg.sender] -= withdrawAmount;
msg.sender.transfer(withdrawAmount);
return "balance"[msg.sender];
}
}


}