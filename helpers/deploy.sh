#!/bin/bash

# source password.sh

USER=coub
ROOT_CRM='..'
# SERVER="$ROOT_CRM/server"
CLIENT="$ROOT_CRM/client"
DESTINATION=vpsfree.2e.cz
# &STACK_DIR=".stack-work/install/x86_64-linux/lts-7.3/8.0.1/bin"
# &INSTALL_DIR="$SERVER/$STACK_DIR"
# tar -hcvzf crm.tgz "$INSTALL_DIR/insert-pass" "$INSTALL_DIR/run-server" "$CLIENT/build"
tar -hcvzf crm.tgz -C $CLIENT "build"
ssh -t $USER@$DESTINATION 'sudo -S mkdir -p /var/www/2e/'
ssh -t $USER@$DESTINATION 'sudo -S chown coub:www-data /var/www/2e/'
scp crm.tgz $USER@$DESTINATION:/var/www/2e/
ssh -t $USER@$DESTINATION 'sudo -S tar xvzf /var/www/2e/crm.tgz -C /var/www/2e/'
ssh -t $USER@$DESTINATION 'sudo -S chown -R coub:www-data /var/www/2e/'
ssh -t $USER@$DESTINATION 'sudo mv /var/www/2e/build /var/www/2e/client/'
# echo $PASS2E | ssh -t $DESTINATION 'sudo -S killall -9 run-server'
