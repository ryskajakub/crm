rm -rf ./archive ;
docker compose --file docker-compose.yml --file docker-compose-build.yml up &&
bash -c 'cd client/ts && yarn && yarn run build' &&
sudo chown -R 1000 ./client/build &&
sudo chown -R 1000 ./server/build &&
sudo chown -R 1000 ./client/ts/build &&
mkdir -p archive/client &&
mkdir -p archive/server &&
mkdir -p archive/tsapp &&
mv ./client/build/* archive/client && 
mv ./client/ts/build/* archive/tsapp && 
mv ./server/build/run-server archive/server &&
cp Dockerfile_run archive/server/Dockerfile &&
docker build --tag ts client/ts &&
docker save ts | gzip -c > crm-ts.tgz &&
tar cvzf crm.tgz ./archive 
