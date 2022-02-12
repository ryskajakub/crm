FROM ubuntu:18.04
RUN apt-get update && apt-get -y install curl libghc-postgresql-libpq-dev libghc-text-icu-dev libghc-gd-dev postgresql-client vim net-tools cabal-install locales npm gulp cpphs
RUN curl -sSL https://get.haskellstack.org/ | sh

RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8

RUN cabal update
RUN cabal install happy-1.19.5 
RUN cabal install fay fay-base fay-text

ENV PATH="/root/.cabal/bin:${PATH}"
RUN mkdir -p /app/server
WORKDIR /app/server
COPY server/stack.yaml stack.yaml
COPY server/server.cabal server.cabal
RUN until stack build --only-snapshot ; do : ; done
RUN stack build --only-snapshot --flag server:build-insert-pass --flag server:build-gen-api --flag server:build-run-server --flag server:build-restart

RUN rm -r * .stack-work
RUN mkdir -p /app/client
RUN mkdir -p /app/shared
ENV PATH="/root/.local/bin:${PATH}"
