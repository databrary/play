#!/bin/sh
set -x
dbName=${1:-databrary-nix-db}
dbPath=$dbName
echo $dbPath
chmod 700 $dbPath/work
# load the schema into the db
cat ./schema/* | gargoyle-psql "$dbPath"
