pragma solidity ^0.4.9;

contract Inbox {
    string lastMessage;

    event Message(
        address indexed from,
        string message
    );

    function Inbox() {
        lastMessage = "inbox empty!";
    }

    function sendMessage(string message) {
        lastMessage = message;
        Message(msg.sender, message);
    }

    function readMessage() returns (string) {
        return lastMessage;
    }
}
