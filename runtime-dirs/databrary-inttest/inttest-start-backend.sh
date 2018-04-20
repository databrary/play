#!/usr/bin/env bash
set -e
# jsCssVersion.txt and empty web directory are still needed to start the backend
# TODO: make flag to start backend only
cat start-backend.ghci | nix-shell --attr databrary-dev.env default.nix --command "cd runtime-dirs/databrary-inttest && ../../ghci-databrary.sh"
