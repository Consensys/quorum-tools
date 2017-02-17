# Build constellation
FROM quorum-stack

RUN apt-get update && \
    apt-get install -y libdb-dev libsodium-dev zlib1g-dev libtinfo-dev && \
    rm -rf /var/lib/apt/lists/*

ENV CONSTELLATION_DIR=/root/constellation

RUN mkdir -p $HOME/.ssh

COPY ./id_noirqs /root/.ssh/id_noirqs

COPY ./github.pub /root/.ssh/known_hosts

RUN git config --global url."git@github.com:".insteadOf "https://github.com/" && \
    echo "    IdentityFile ~/.ssh/id_noirqs" >> /etc/ssh/ssh_config && \
    ssh-agent bash -c '\
      ssh-add $HOME/.ssh/id_noirqs; \
      git clone git@github.com:jpmorganchase/constellation.git $CONSTELLATION_DIR; \
      cd $CONSTELLATION_DIR; \
      git reset --hard c3031121 && stack --resolver lts-7.14 build; \
    '
