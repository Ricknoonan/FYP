pragma solidity ^0.5.0;

contract Greeter {

    function sayHello() public pure returns(string memory) {
            return "Hello World!";
    }
}

    constructor (string memory initialMessage) public {
            message=initialMessage;
    }