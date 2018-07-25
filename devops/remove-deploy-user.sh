#!/usr/bin/env bash
set -e

# this script deletes a user from staging or prod which formerly served as the nix user

# stop databrary process
rm /home/$APPUSER/databraryExeLink
sudo gpasswd -d $RMUSER databrary
# delete sourcing nix from .bash_profile
# delete sourcing nix from .bashrc
