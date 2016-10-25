#!/bin/bash

source password.sh

ROOT_CRM='..'
SERVER="$ROOT_CRM/server"
CLIENT="$ROOT_CRM/client"
DESTINATION=vpsfree.2e.cz
STACK_DIR=".stack-work/install/x86_64-linux/lts-7.3/8.0.1/bin"
INSTALL_DIR="$SERVER/$STACK_DIR"
tar -hcvzf crm.tgz "$INSTALL_DIR/insert-pass" "$INSTALL_DIR/run-server" "$CLIENT/build"
echo $PASS2E | ssh -t $DESTINATION 'sudo -S mkdir -p /var/www/2e/'
echo $PASS2E | ssh -t $DESTINATION 'sudo -S chown coub:www-data /var/www/2e/'
scp crm.tgz $DESTINATION:/var/www/2e/
echo $PASS2E | ssh -t $DESTINATION 'sudo -S tar xvzf /var/www/2e/crm.tgz -C /var/www/2e/'
echo $PASS2E | ssh -t $DESTINATION 'sudo -S chown -R coub:www-data /var/www/2e/'
echo $PASS2E | ssh -t $DESTINATION 'sudo -S killall -9 run-server'
