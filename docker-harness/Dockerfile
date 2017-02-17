# Build the harness
FROM quorum-stack

ENV GEMINI_EXAMPLES_DIR=/root/gemini-examples

RUN mkdir -p $HOME/.ssh

COPY ./id_noirqs /root/.ssh/id_noirqs

COPY ./github.pub /root/.ssh/known_hosts

RUN git config --global url."git@github.com:".insteadOf "https://github.com/" && \
    echo "    IdentityFile ~/.ssh/id_noirqs" >> /etc/ssh/ssh_config && \
    ssh-agent bash -c '\
      ssh-add $HOME/.ssh/id_noirqs; \
      git clone git@github.com:noirqs/gemini-examples.git $GEMINI_EXAMPLES_DIR; \
      cd $GEMINI_EXAMPLES_DIR; \
      git reset --hard 95c0864d && cd raft-demo && stack --resolver lts-7.14 build; \
    '
