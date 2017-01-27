from ubuntu:xenial

ENV GOPATH=/root/go \
    GO_ETHEREUM_DIR=/root/go/src/github.com/ethereum/go-ethereum

RUN apt-get update && \
    apt-get install -y ssh git golang && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p $HOME/.ssh

COPY ./id_noirqs /root/.ssh/id_noirqs

COPY ./github.pub /root/.ssh/known_hosts

RUN git config --global url."git@github.com:".insteadOf "https://github.com/" && \
    echo "    IdentityFile ~/.ssh/id_rsa" >> /etc/ssh/ssh_config && \
    ssh-agent bash -c '\
      ssh-add $HOME/.ssh/id_noirqs; \
      git clone ssh://git@github.com/noirqs/jpm-goethereum.git $GO_ETHEREUM_DIR; \
      cd $GO_ETHEREUM_DIR; \
      git reset --hard bc2cb20b && make all && cp build/bin/geth /usr/local/bin; \
    '
