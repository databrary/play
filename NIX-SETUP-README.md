DISCLAIMER: This is a proto-type document for development. Do not use. 

This will guide a NIX-HASKELL DEVELOPER through the setup got Databrary so
far, please update this document with fixes as they are deployed. 

----------------------------------------------------------------------------
INTERNAL NOTE: A script should be added to handle pre-requesites and part of
the pre-building process. Include:
    *(Check/Download) Postgresql
    *(Check/Download) Solr
      --solr-6.6.0 has been added to this repository
      --Run: 
      ```bash
      $solr-6.6.0/bin/solr start
      $solr-6.6.0/bin/solr create -c databrary_solr_store
      ```
      To start solr and create the solr store:
      TODO: Import schema and Items from solr/ directory

    *(Check/Download) Cabal
---------------------------------------------------------------------------
Welcome to the Jungle! Clone the Databrary Repo and checkout the
obsidian-develop branch. 
```bash
$git clone git@github.com:databrary/databrary.git
$git checkout obsidian-develop
```

STEP 1: 
  From the root directory of this project, run 
  ```bash 
  $nix-shell
  $./confgen.hs
  $./bar
  ``` 
  The first command set up the databrary.conf file
  The script that followed set up the postgresql server along with required schemas

STEP 2:
  From the same root directory of this project inside the nix-shell, run 
  ```bash
  $cabal repl databrary
  ```

STEP 3: 
  When cabal successfully builds all 303 dependencies, run 
  ```bash
  $main
  ```
Point your browser to localhost:8000 and observe the current status of the
application. 

Note: In order to clean your database & cabal test environment, it is best to
execute the following commands in this order: 
```bash
$dropdb databrary
$dropuser databrary
$pg_ctl -D databrary-db -l logfile stop
$rm -rf databrary-db/
$rm -rf dist/
```
