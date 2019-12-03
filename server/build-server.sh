#!/bin/bash
stack install \
  --flag server:build-insert-pass \
  --flag server:build-gen-api \
  --flag server:build-run-server \
  --flag server:build-restart \
  --file-watch \
  --exec "./restart-server.sh"
