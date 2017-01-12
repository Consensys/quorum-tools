from ubuntu:xenial

# (these are all fully expanded because I think you can't reference environment
# variables from within an ENV declaration)
ENV GOPATH=/root/go \
    GO_ETHEREUM_DIR=/root/go/src/github.com/ethereum/go-ethereum \
    GEMINI_EXAMPLES_DIR=/root/gemini-examples
    # PATH=/root/go/...

# install stack:
    # get fp complete key
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442 && \
    # add stack deb repo
    echo 'deb http://download.fpcomplete.com/ubuntu xenial main' | tee /etc/apt/sources.list.d/fpco.list && \

    apt-get update && apt-get install -y \
      make \
      ssh \
      git \
      golang \
      stack \
      wrk && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p $HOME/.ssh

# TODO: why didn't this work with destination `$HOME/.ssh/id_noirqs`?
COPY ./id_noirqs /root/.ssh/id_noirqs

# set up ssh, then build ethereum and raft-demo
RUN git config --global url."git@github.com:".insteadOf "https://github.com/" && \
    echo "    IdentityFile ~/.ssh/id_rsa" >> /etc/ssh/ssh_config && \
    echo "StrictHostKeyChecking no " > $HOME/.ssh/config && \
    touch $HOME/.ssh/known_hosts && \
    ssh-keyscan -t rsa,dsa,ecdsa github.com >> $HOME/.ssh/known_hosts && \

    ssh-agent bash -c '\
      ssh-add $HOME/.ssh/id_noirqs; \
      git clone ssh://git@github.com/noirqs/etcd.git $GO_PATH/src/github.com/coreos/etcd; \
      git clone ssh://git@github.com/noirqs/jpm-goethereum.git $GO_ETHEREUM_DIR; \
      cd $GO_ETHEREUM_DIR && git fetch && git checkout jpm-raft && make all; \
      git clone ssh://git@github.com/noirqs/gemini-examples.git $GEMINI_EXAMPLES_DIR; \
      cd $GEMINI_EXAMPLES_DIR/raft-demo && stack setup && stack build; \
    '
