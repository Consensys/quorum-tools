# Set up stack without actually building the project
FROM quorum-raft

ENV GEMINI_EXAMPLES_DIR=/root/gemini-examples

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442 && \
    echo 'deb http://download.fpcomplete.com/ubuntu xenial main' | tee /etc/apt/sources.list.d/fpco.list && \
    apt-get update && \
    apt-get install -y make ssh git stack wrk && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p $HOME/.ssh

COPY ./id_noirqs /root/.ssh/id_noirqs

COPY ./github.pub /root/.ssh/known_hosts

# we pre-install a bunch of dependencies (not currently all of them though we could add more)
RUN git config --global url."git@github.com:".insteadOf "https://github.com/" && \
    echo "    IdentityFile ~/.ssh/id_noirqs" >> /etc/ssh/ssh_config && \
    ssh-agent bash -c '\
      stack --resolver lts-8.3 --install-ghc install text mtl async managed foldl lens lens-aeson bytestring turtle aeson wreq safe transformers containers time http-client; \
    '
