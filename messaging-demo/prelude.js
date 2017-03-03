// keys duplicated from credentials for convenience
var key1 = "BULeR8JyUWhiuuCMU/HLA0Q5pzkYT+cHII3ZKBey3Bo=";
var key2 = "QfeDAys9MPDs2XHExtc84jKGHxZg/aj52DTh0vtA3Xc=";
var key3 = "1iTZde/ndBHvzhcl7V68x44Vx7pl8nwx9LqnM/AfJUg=";

var contract = eth.contract([{"constant":false,"inputs":[{"name":"message","type":"string"}],"name":"sendMessage","outputs":[],"payable":false,"type":"function"},{"anonymous":false,"inputs":[{"indexed":true,"name":"from","type":"address"},{"indexed":false,"name":"message","type":"string"}],"name":"Message","type":"event"}]);

var data = '6060604052341561000c57fe5b5b6101868061001c6000396000f30060606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063469c81101461003b575bfe5b341561004357fe5b610093600480803590602001908201803590602001908080601f01602080910402602001604051908101604052809392919081815260200183838082843782019150505050505091905050610095565b005b3373ffffffffffffffffffffffffffffffffffffffff167f811f7cff0a3374ff67cccc3726035d34ba70410e0256818a891e4d6acc01d88e82604051808060200182810382528381815181526020019150805190602001908083836000831461011d575b80518252602083111561011d576020820191506020810190506020830392506100f9565b505050905090810190601f1680156101495780820380516001836020036101000a031916815260200191505b509250505060405180910390a25b505600a165627a7a72305820e13a72f97a40a19fc2254edf6335fb401060f65b4bf24fe1b8ec440681437a1f0029';

var from = eth.accounts[0];
var gas = '4700000';

function createInbox(privateFor) {
  var messageBox = contract.new({
    from: from,
    data: data,
    gas: gas,
    privateFor: privateFor
  }, function (e, contract) {
    if (typeof contract.address !== 'undefined') {
      console.log('Contract mined!');
      console.log('address: ' + contract.address);
      console.log('transactionHash: ' + contract.transactionHash);
      console.log('tell your friend to run listen("' + contract.address + '")');
    }
  });

  return function send(msg) {
    messageBox.sendMessage(msg, {
      from: from,
      privateFor: privateFor,
      gas: gas,
    });
  }
}

function listen(address) {
  contract.at(address).Message(function(error, result) {
    if (error) {
      console.log(error);
    } else {
      // console.log(result.args.from);
      console.log(result.args.message);
    }
  });
}
