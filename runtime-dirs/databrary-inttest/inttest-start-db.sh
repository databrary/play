#!/usr/bin/env bash
set -e

cd $(git rev-parse --show-toplevel)
nix-shell --attr databrary-dev.env default.nix --command "cd runtime-dirs/databrary-inttest && ./init-db"
