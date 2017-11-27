# Databrary

Guide for setting up a development environment for Databrary.

---------------------------------------------------------------------------
Welcome to the Jungle!

### STEP 0
From the root directory of this project, run
```bash
# If you don't already have nix, get it! (If you do have it, skip this step)
> ./setup-nix

# If you just want to add the binary caches to your nix config (note that this is run by setup-nix, so you don't need to run both):
> sudo ./setup-binary-caches
```

### STEP 1
From the root directory of this project, run
```bash
# Enter environment for running postgres database
$ nix-shell

# Set up the postgresql database along with required schemas
# This is a synchronous procedure, use Ctrl-d to close psql and shutdown database.
[nix-shell:..]$ ./init-db-psql.sh
```
Note: Ensure Ctrl-d is used to shutdown psql and database when editing is complete. If you encounter any errors, use 
 `ps aux | grep postgres` to find if a previous running postgresql database is still running and kill appropriately. 

### STEP 2
In a separate shell, from the same root directory of this project, run
```bash
# Start Haskell interactive interpreter, within an environment sandboxed for building/running databrary.
# If missing, create directories and download dependencies, before launching interpreter.
$ nix-shell --run ghci-databrary
```
Note: There will be some delay the first time this runs, as it downloads large packages initially.

### STEP 3
When Haskell interpreter finishes loading, run
```bash
# Bring Main module into scope
*...> :m + Main

# Generate frontend assets
*... Main> :main -w

# Start databrary backend
*... Main> main
```

Point your browser to localhost:8000

#### -----HOT RELOAD FOR FRONTEND DEVELOPERS-----

After running the steps provided above, in a separate terminal you can run the following script 
to watch for any changes the developer makes to frontend files located within the 'web/' directory: 
```bash
$ ./hotreload.sh 
```
Note: If you're going to apply a batch change to multiple frontend source files(e.g., git checkout), 
stop the hot-reload script

#### -----HOT RELOAD(ghcid) FOR BACKEND HASKELL DEVELOPERS-----

After running 'Step 1'  provided above, in a separate terminal you can run the following commands 
to watch for any changes the developer makes to backend files located within the 'Databrary/' directory: 
```bash
$nix-shell
$ ./ghcid-backend
```

#### -----Local Database Management-----

The `init-db` script used above, by default, will create a database directory called `databrary-nix-db`.
If a different database directory name is desired, `init-db` takes a single argument that will become the
name of the database dir. 

To remove or drop the entire local database. Simply remove the database directory with an `$rm -rf` command. 


#### Sign-Up | Log-In | Email Confirmation

In order Sign up for Databrary within your test envirionment, you will need to
be able to receive the confirmation email. 

Emails have been configured to redirect to https://mailtrap.io/ (MailTrap), an 
email test environment. Here you can be invited to the shared email trap, or
you can create and use your own credentials. 

To add/modify email credentials of your Databrary developer environment, edit
the config/email document found in the root of the project directory.
 
Once you have confirmed your account will need to be approved by an
administrative user. There are two predefined administrative accounts: 
  
  1. admin@databrary.org
  2. test@databrary.org

Passwords for these accounts are given to qualified developers upon request.  



## License

Copyright (C) 2013-2016 New York University

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
