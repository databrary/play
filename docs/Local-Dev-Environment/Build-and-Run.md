Note: The commands below are intentionally verbose currently, to
encourage people to understand what they are using, so that we become
more fluent and understand how to customize further when needed.

## Initial startup

### Step 0

From anywhere on your machine, run the following to download the nix
setup script and run it. When this is complete, you will have the base
nix commands installed on your machine. Your system will also be
configured to be able to connect to a public nix cache as well as our
additional private nix
    cache.

    $ curl -L https://rawgit.com/databrary/databrary/master/setup-nix/0-sudo.sh | bash
    $ curl -L https://rawgit.com/databrary/databrary/master/setup-nix/1.sh | bash

### Step 1

From the root directory of this project, run

    # Start the postgresql server along with creating required schemas, if missing.
    # Note: use Ctrl-d or \q to close psql and shutdown database.
    $ nix-shell --run ./init-db-pql.sh

Note: When you have finished your development session, **make sure to
stop the interpreter from step 2 first**, then use Ctrl-d or \\q is used
to shutdown psql and database. See "note 1" below for more information.

### Step 2

In a separate shell, from the same root directory of this project, run

    # Start Haskell interactive interpreter, within an environment sandboxed for building databrary.
    # (If missing, creates directories and downloads dependencies, before launching interpreter.)
    $ nix-shell --run ./ghci-databrary.sh


    # Once interpreter has finished loading, run the following command.
    # (This brings Main module into scope, reloads, generates frontend assets, including constants and routes, and starts databrary backend)
    *...> :script reload-gen-start

Note: There will be some delay the first time this runs after cloning,
as it downloads large packages initially.

Point your browser to localhost:8000.

-----

### Hot reload for frontend developers

After running the steps provided above, in a separate terminal you can
run the following script to watch for any changes the developer makes to
frontend files located within the 'web/' directory:

    # Re-generate front end assets, excluding constants and routes, each time a file is changed
    $ ./hotreload.sh


Note: If you're going to apply a batch change to multiple frontend
source files(e.g., git checkout), stop the hot-reload script.

Note 2: You can also use :script gen-start instead of :script
reload-gen-start to start the server if no reload is needed after
stopping.

If hot reload is failing, you can workaround as follows. After each
batch of editing is complete (from outside of a nix-shell), manually
run:

    # Re-generate front end assets, excluding constants and routes
    $ ./generate.sh


### Applying new database migrations

  - In local development, currently the entire set of database
    migrations are only run when the database is first created
  - In order to receive new database migrations, we need to delete the
    current db and allow the automatic database creation to run
  - Ensure that hot reload has been stopped
  - Ensure that the web application, haskell interpreter, and containing
    nix-shell have been stopped
  - Ensure that the database and the nix shell running it has been
    stopped
  - From inside of your clone of databrary, delete the databrary-nix-db
    folder:


        $ rm -rf databrary-nix-db/


  - Note: databrary-nix-db contains all the files that constitute the
    locally created postgres database. By deleting the folder, you have
    deleted the
  - Next time you start the database normally, it will create a new
    database will all migrations applied


### Local Database Management

To remove or drop the entire local database. Simply remove the database
directory with an \`$rm -rf\` command.



-----

#### Note 1

If you see the following only, then the database has not completely
shutdown. Mostly likely, the interpreter from step 2 is still running
somewhere.

    postgres=# \q
    LOG: received smart shutdown request
    LOG: autovacuum launcher shutting down
    [nix-shell:~/dev/databrary]$

The database has only shut down when you have seen these two additional
lines:

    LOG: shutting down
    LOG: database system is shut down


If you encounter any errors generally when attempting to start the
database, use the following procedure

    # check if database is running
    $ head -1 databrary-nix-db/work/postmaster.pid

    # kill the old running database, if you observed it was running in previous step
    $ kill `head -1 databrary-nix-db/work/postmaster.pid`
