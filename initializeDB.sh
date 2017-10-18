#!/bin/sh 
set -x
set -v
dbName=${1:-databrary-nix-db}
dbPath=$dbName
echo $dbPath
gargoyle-psql "$dbPath" <<-EOSQL
     CREATE USER databrary;
     CREATE DATABASE databrary;
     GRANT ALL PRIVILEGES ON DATABASE databrary TO databrary;
     ALTER USER databrary WITH PASSWORD 'databrary123';
     ALTER USER databrary WITH SUPERUSER;
EOSQL
chmod 700 $dbPath/work
echo "HERE IS WHAT IS IN /work..."
ls -al $dbPath/work
cat ./schema/* | gargoyle-psql "$dbPath" 
