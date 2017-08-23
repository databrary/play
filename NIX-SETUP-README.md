DISCLAIMER: This is a proto-type document for development.

Guide for setting up a development environment for Databrary.
Please update this document with fixes as they are deployed.

---------------------------------------------------------------------------
Welcome to the Jungle!

Clone the Databrary Repo and checkout the obsidian-develop branch. 
```bash
> git clone git@github.com:databrary/databrary.git
> git checkout obsidian-develop
```

### STEP 1
From the root directory of this project, run
```bash
# If you don't already have nix, get it! (If you do have it, skip this step)
> ./setup-nix
# If you just want to add the binary caches to your nix config (note that this is run by setup-nix, so you don't need to run both):
> sudo ./setup-binary-caches
# Enter environment for building/running databrary
> nix-shell
# Set up the postgresql database along with required schemas
> ./init-db
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

Note: In order to stop your database, you can:
```bash
./stop-db
```

