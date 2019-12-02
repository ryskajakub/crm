FROM ubuntu:18.04
RUN apt-get update && apt-get -y install curl libghc-postgresql-libpq-dev libghc-text-icu-dev libghc-gd-dev postgresql-client vim net-tools cabal-install && curl -sSL https://get.haskellstack.org/ | sh
RUN cabal update
RUN cabal install happy-1.19.5
ENV PATH="~/.cabal/bin:${PATH}"
RUN mkdir -p /app/server
WORKDIR /app/server
COPY server/stack.yaml stack.yaml
COPY server/server.cabal server.cabal
RUN until stack build --only-snapshot ; do : ; done

COPY server server
RUN sleep 1000000000
# RUN stack build --flag server:build-run-server 
