#!/bin/bash
cd "$(dirname "$0")"

ARGS="$@"

docker run --user=node --network=host --workdir=/app -v $(pwd):/app --rm --name=crm-runner node:12-buster bash -c "$ARGS"
