#!/usr/bin/env bash
set -eu

# perform sudo tasks first
if ! [ -e /nix ]; then
    echo "Creating /nix directory using sudo"
    sudo mkdir -m 0755 /nix
fi
echo "Changing owner of /nix to $USER using sudo"
sudo chown -R $USER /nix

if ! [ -e /etc/nix ]; then
    echo "Creating /etc/nix to house nix.conf using sudo"
    sudo mkdir -p /etc/nix
fi
echo "Creating /etc/nix/nix.conf using sudo (overwriting any previous version)"
if [ -e /etc/nix/nix.conf ]; then
    echo "Backing up nix.conf"
    sudo cp /etc/nix/nix.conf /etc/nix/nix.conf.backup
fi
# is this setting actually used? it appeared to use an incorrect path before
sudo sh -c 'echo "secret-key-files = /home/datadeploy/devdatabrary2-nix-store.priv" > /etc/nix/nix.conf'








