#!/bin/sh
set -x
dbName=${1:-databrary-nix-db}
dbPath=$dbName
echo $dbPath
chmod 700 $dbPath/work
# load the schema into the db
# subtly relies on cat listing in deterministic, ascending order
cat ./schema/*.sql | gargoyle-psql "$dbPath"
