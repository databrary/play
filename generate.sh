#!/usr/bin/env bash

# This script builds the 'generate' executable used in the hot-reloading
# of frontend web assets. 

set -eu

if [ ! -f ./dist/build/generate/generate ]; then
  nix-shell --command "cabal build generate"
fi

nix-shell --command 'databrary_datadir=. ./dist/build/generate/generate'
RESULT=$?
NOW=`date`
if [ $RESULT -eq 0 ]
  then
    echo -e "\n\e[7m[$NOW] Frontend files generated\e[0m\n"
  else
    echo -e "\n\e[7m[$NOW] FAILED to generate frontend files!\e[0m\n"
fi
