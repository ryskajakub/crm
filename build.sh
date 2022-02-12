docker-compose --file docker-compose.yml --file docker-compose-server-build.yml --file docker-compose-fay-build.yml up &&
sudo chown -R coub:coub ./client/build &&
mkdir -p archive/server
mkdir -p archive/client
mv ./client/build/* archive/client
mv ./server/build/run-server archive/server
cp Dockerfile_run archive/server
tar cvzf crm.tgz ./archive
