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
> databrary_datadir=. cabal repl databrary
```
Note: provide datadir to workaround issue with auto-generated Paths_databrary module in cabal repl

### STEP 3
When cabal repl finishes loading, run
```bash
#Bring Main module into scope
> :m + Main

# Generate frontend assets
> :main -w

# Start databrary backend
> main
```

Point your browser to localhost:8000

-----HOT RELOAD FOR FRONTEND DEVELOPERS-----

After running the steps provided above, in a separate terminal you can run the following script 
to watch for any changes the developer makes to frontend files located within the 'web/' directory: 
```bash
$ ./hotreload.sh 
```

-----HOT RELOAD(ghcid) FOR BACKEND HASKELL DEVELOPERS-----
After running 'Step 1'  provided above, in a separate terminal you can run the following commands 
to watch for any changes the developer makes to backend files located within the 'Databrary/' directory: 
```bash
$nix-shell
$ ./ghcid-backend
```


Sign-Up | Log-In | Email Confirmation

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
