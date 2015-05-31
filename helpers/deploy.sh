#!/bin/bash

ROOT_CRM='..'
SERVER="$ROOT_CRM/server"
CLIENT="$ROOT_CRM/client"
tar -hcvzf crm.tgz "$SERVER/.cabal-sandbox/bin/gen-pass" "$SERVER/.cabal-sandbox/bin/run-server" "$CLIENT/build" crm
scp crm.tgz mywedos:/var/www/2e/
ssh mywedos 'tar xvzf /var/www/2e/crm.tgz -C /var/www/2e/'
ssh mywedos 'killall -9 run-server'
ssh mywedos '/var/www/2e/server/.cabal-sandbox/bin/run-server'
