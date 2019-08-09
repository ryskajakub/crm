#!/bin/bash
stack install --flag server:build-run-server --flag server:build-insert-pass --extra-lib-dirs=/usr/local/opt/icu4c/lib --extra-include-dirs=/usr/local/opt/icu4c/include
