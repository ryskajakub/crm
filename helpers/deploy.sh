#!/bin/bash

tar -hcvzf crm.tgz deploy-dir/
scp crm.tgz mywedos:/var/www/2e/
ssh mywedos 'tar xvzf /var/www/2e/crm.tgz -C /var/www/2e/'
