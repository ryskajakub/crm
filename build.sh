rm -rf ./archive ;
docker-compose --file docker-compose.yml --file docker-compose-server-build.yml --file docker-compose-fay-build.yml up &&
sudo chown -R coub:coub ./client/build &&
sudo chown -R coub:coub ./server/build &&
mkdir -p archive/client &&
mkdir -p archive/server &&
mv ./client/build/* archive/client && 
mv ./server/build/run-server archive/server &&
cp Dockerfile_run archive/server/Dockerfile &&
tar cvzf crm.tgz ./archive
