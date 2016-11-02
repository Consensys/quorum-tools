# Run these commands by hand!

sudo apt install golang fish wrk parallel
ssh-keygen -o -a 100 -t ed25519
eval "$(ssh-agent -s)"
ssh-add .ssh/id_ed25519
# add to github
git config --global url."git@github.com:".insteadOf "https://github.com/"
mkdir -p go/src/github.com/ethereum go/src/github.com/coreos
cat >> .bashrc <<- EOM
export GOPATH=$HOME/go
export PATH=$HOME/go/bin:$PATH
EOM
source .bashrc
git clone git@github.com:noirqs/gemini-examples.git
cd go/src/github.com
git clone git@github.com:bts/etcd.git coreos/etcd
git clone git@github.com:bts/rafthereum.git ethereum/go-ethereum
go get ./ethereum/go-ethereum/cmd/geth
