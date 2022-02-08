docker-compose --file docker-compose-fay.yml --file docker-compose-fay-build.yml up &&
sudo rsync --owner --group --chown=nginx:nginx --chmod=D775,F664 --recursive client/build/ /var/www/program/ &&
docker-compose --file docker-compose-main.yml --file docker-compose-prod.yml up