#!/bin/sh 
set -x
dbName=databrary-nix-db
dbPath=$dbName
echo $dbPath
chmod 700 $dbPath/work
gargoyle-psql "$dbPath" <<-EOSQL
     CREATE USER databrary;
     CREATE DATABASE databrary;
     GRANT ALL PRIVILEGES ON DATABASE databrary TO databrary;
     ALTER USER databrary WITH PASSWORD 'databrary123';
     ALTER USER databrary WITH SUPERUSER;
EOSQL

cat ./schema/* | gargoyle-psql "$dbPath" 
gargoyle-psql "$dbPath" & 

