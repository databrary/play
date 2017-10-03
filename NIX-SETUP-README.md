DISCLAIMER: This is a proto-type document for development.

Guide for setting up a development environment for Databrary.
Please update this document with fixes as they are deployed.

---------------------------------------------------------------------------
Welcome to the Jungle!

Clone the Databrary Repo and checkout the obsidian-develop branch. 
```bash
> git clone https://github.com/databrary/databrary.git
> cd databrary
> git checkout obsidian-develop

```

### STEP 1
From the root directory of this project, run
```bash
# If you don't already have nix, get it! (If you do have it, skip this step)
> ./setup-nix
# After it completes run the following to update environment or restart the shell
> source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
# If you just want to add the binary caches to your nix config (note that this is run by setup-nix, so you don't need to run both):
> sudo ./setup-binary-caches
# Enter environment for building/running databrary
> nix-shell
# For MacOSX, please delete nixpkgs.postfix line from shell.nix
#
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
# for reload, to cause Haskell to rebuild
> :r

# Generate frontend assets
> :main -w

# Start databrary backend
> main
```

Point your browser to localhost:8000

Note: In order to stop your database, you can:
```bash
./stop-db
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
