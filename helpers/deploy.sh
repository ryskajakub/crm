#!/bin/bash

ROOT_CRM='..'
SERVER="$ROOT_CRM/server"
CLIENT="$ROOT_CRM/client"
STACK_DIR=".stack-work/install/x86_64-linux/lts-2.15/7.8.4/bin"
INSTALL_DIR="$SERVER/$STACK_DIR"
tar -hcvzf crm.tgz "$INSTALL_DIR/insert-pass" "$INSTALL_DIR/run-server" "$CLIENT/build" crm
scp crm.tgz mywedos:/var/www/2e/
ssh mywedos 'tar xvzf /var/www/2e/crm.tgz -C /var/www/2e/'
ssh mywedos 'killall -9 run-server'
ssh mywedos "/var/www/2e/server/$STACK_DIR/run-server"
