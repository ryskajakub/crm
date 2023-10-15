rm -rf ./archive ;
docker-compose --file docker-compose.yml --file docker-compose-server-build.yml --file docker-compose-fay-build.yml up &&
bash -c 'cd client/ts && yarn run build' &&
sudo chown -R coub:coub ./client/build &&
sudo chown -R coub:coub ./server/build &&
sudo chown -R coub:coub ./client/ts/build &&
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
