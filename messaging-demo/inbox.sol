pragma solidity ^0.4.9;

contract Inbox {
    event Message(
        address indexed from,
        string message
    );

    function sendMessage(string message) {
        Message(msg.sender, message);
    }
}
