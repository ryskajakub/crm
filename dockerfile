FROM ubuntu:18.04
RUN apt-get update && apt-get -y install curl libghc-postgresql-libpq-dev libghc-text-icu-dev libghc-gd-dev && curl -sSL https://get.haskellstack.org/ | sh
COPY ./server/stack.yaml /app/server/
COPY ./server/server.cabal /app/server/
WORKDIR /app/server
RUN stack setup
RUN until stack build --only-snapshot ; do : ; done
# change here
RUN apt-get update && apt-get install -y postgresql-client vim net-tools
WORKDIR /app/server
COPY . /app
# RUN stack build --flag server:build-run-server 
