FROM ubuntu:18.04
RUN apt-get update && apt-get -y install curl libghc-postgresql-libpq-dev libghc-text-icu-dev libghc-gd-dev postgresql-client vim net-tools cabal-install && curl -sSL https://get.haskellstack.org/ | sh
RUN cabal update
RUN cabal install happy-1.19.5
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

ENTRYPOINT sleep 1000000000
# RUN stack build --flag server:build-run-server 
