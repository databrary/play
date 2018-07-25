#!/usr/bin/env bash
set -eu

CACHE_USER="datadeploy"
# perform sudo tasks first
if ! [ -e /nix ]; then
    echo "Creating /nix directory using sudo"
    sudo mkdir -m 0755 /nix
fi
echo "Changing owner of /nix to $USER using sudo"
sudo chown -R $CACHE_USER /nix

# this might be cleaner than how nix-serve is currently started...
# if ! [ -e /etc/nix ]; then
#     echo "Creating /etc/nix to house nix.conf using sudo"
#     sudo mkdir -p /etc/nix
# fi
# echo "Creating /etc/nix/nix.conf using sudo (overwriting any previous version)"
# if [ -e /etc/nix/nix.conf ]; then
#     echo "Backing up nix.conf"
#     sudo cp /etc/nix/nix.conf /etc/nix/nix.conf.backup
# fi
# sudo sh -c 'echo "secret-key-files = /home/datadeploy/devdatabrary2-nix-store.priv" > /etc/nix/nix.conf'
