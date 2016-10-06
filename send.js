var myAccount = eth.accounts[0]
personal.unlockAccount(myAccount, 'abcd');
web3.eth.defaultAccount = myAccount;

var accounts = {};

// API

var registerAccounts = function(accts) {
    accounts = accts;
};

var sendTo = function(recipientName) {
    var recipientAccount = accounts[recipientName];

    var txHash = eth.sendTransaction({
        from: myAccount,
        to: recipientAccount,
        value: web3.toWei(1, "ether")
    })

    return txHash;
};
