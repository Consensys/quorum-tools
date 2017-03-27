# Build the harness
FROM quorum-stack

ENV QUORUM_TOOLS_DIR=/root/quorum-tools

RUN mkdir -p $HOME/.ssh

COPY ./id_noirqs /root/.ssh/id_noirqs

COPY ./github.pub /root/.ssh/known_hosts

RUN git config --global url."git@github.com:".insteadOf "https://github.com/" && \
    echo "    IdentityFile ~/.ssh/id_noirqs" >> /etc/ssh/ssh_config && \
    ssh-agent bash -c '\
      ssh-add $HOME/.ssh/id_noirqs; \
      git clone git@github.com:noirqs/quorum-tools.git $QUORUM_TOOLS_DIR; \
      cd $QUORUM_TOOLS_DIR; \
      git reset --hard 7796ba2 && cd raft-demo && stack --resolver lts-8.3 build; \
    '
