#!/bin/bash

ROOT_CRM='..'
SERVER="$ROOT_CRM/server"
CLIENT="$ROOT_CRM/client"
DESTINATION=program.2e.cz
STACK_DIR=".stack-work/install/x86_64-linux/lts-3.13/7.10.2/bin"
INSTALL_DIR="$SERVER/$STACK_DIR"
tar -hcvzf crm.tgz "$INSTALL_DIR/insert-pass" "$INSTALL_DIR/run-server" "$CLIENT/build" crm
scp crm.tgz $DESTINATION:/var/www/2e/
ssh $DESTINATION 'tar xvzf /var/www/2e/crm.tgz -C /var/www/2e/'
ssh $DESTINATION 'sudo killall -9 run-server'
