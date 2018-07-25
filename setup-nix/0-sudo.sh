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
sudo sh -c 'echo "binary-caches = https://cache.nixos.org http://devdatabrary2.home.nyu.edu:5000/" > /etc/nix/nix.conf'
sudo sh -c 'echo "binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= devdatabrary2.home.nyu.edu-1:xpI1XOvf7czNv0+0/1ajpgotpOnUMTUBBF9v97D5/yk=" >> /etc/nix/nix.conf'
