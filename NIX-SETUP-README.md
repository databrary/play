DISCLAIMER: This is a proto-type document for development.

Guide for setting up a development environment for Databrary.
Please update this document with fixes as they are deployed.

---------------------------------------------------------------------------
Welcome to the Jungle! Clone the Databrary Repo and checkout the
obsidian-develop branch. 
```bash
> git clone git@github.com:databrary/databrary.git
> git checkout obsidian-develop
```

### STEP 1
From the root directory of this project, run
```bash
# Enter environment for building/running databrary
> nix-shell

# Set up the postgresql database along with required schemas
> ./bar
```

### STEP 2
From the same root directory of this project inside the nix-shell, run
```bash
> cabal repl databrary
```

### STEP 3
When cabal repl finishes loading, run
```bash
# Generate frontend assets
> :main -w

# Start databrary backend
> main
```

Point your browser to localhost:8888


Note: In order to clean your database & cabal test environment, it is best to
execute the following commands in this order: 
```bash
$dropdb databrary
$dropuser databrary
$pg_ctl -D databrary-db -l logfile stop
$rm -rf databrary-db/
$rm -rf dist/
```

----------------------------------------------------------------------------
INTERNAL NOTE: A script should be added to handle pre-requesites and part of
the pre-building process. Include:
    *(Check/Download) Cabal
