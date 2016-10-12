var myAccount = eth.accounts[0]
personal.unlockAccount(myAccount, 'abcd');
web3.eth.defaultAccount = myAccount;

var accounts = {};

// API

var registerAccounts = function(accts) {
  accounts = accts;
};

var sendTo = function(recipientName, etherAmount) {
  var recipientAccount = accounts[recipientName];

  var txHash = eth.sendTransaction({
    from: myAccount,
    to: recipientAccount,
    value: web3.toWei(etherAmount, "ether")
  });

  return txHash;
};

var sendManyTo = function(recipientName, numberTxes) {
    var recipientAccount = accounts[recipientName];
    var txDesc = {
      from: myAccount,
      to: recipientAccount,
      value: web3.toWei(0.000000001, "ether")
    };

    for (var i = 0; i < numberTxes; i++) {
      eth.sendTransaction(txDesc);
    }
};

var batchTo = function(recipientName, numberTxes) {
  var recipientAccount = accounts[recipientName];

  return eth.repeatedlySendTransaction({
    from: myAccount,
    to: recipientAccount,
    value: web3.toWei(0.000000001, "ether")
  }, numberTxes);
};
