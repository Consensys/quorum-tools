from ubuntu:xenial

ENV GEMINI_EXAMPLES_DIR=/root/gemini-examples

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442 && \
    echo 'deb http://download.fpcomplete.com/ubuntu xenial main' | tee /etc/apt/sources.list.d/fpco.list && \
    apt-get update && \
    apt-get install -y make ssh git stack wrk && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p $HOME/.ssh

COPY ./id_noirqs /root/.ssh/id_noirqs

COPY ./github.pub /root/.ssh/known_hosts

RUN git config --global url."git@github.com:".insteadOf "https://github.com/" && \
    echo "    IdentityFile ~/.ssh/id_rsa" >> /etc/ssh/ssh_config && \
    ssh-agent bash -c '\
      ssh-add $HOME/.ssh/id_noirqs; \
      git clone ssh://git@github.com/noirqs/gemini-examples.git $GEMINI_EXAMPLES_DIR; \
      cd $GEMINI_EXAMPLES_DIR/raft-demo && stack setup && stack build; \
    '
