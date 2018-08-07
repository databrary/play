#!/usr/bin/env bash
set -e
# jsCssVersion.txt and empty web directory are still needed to start the backend
# TODO: make flag to start backend only
nix-shell --attr databrary-dev.env default.nix --command 'cd runtime-dirs/databrary-unit-impure && ../../ghci-databrary.sh --skip-ghci'
nix-shell --attr databrary-dev.env default.nix --command 'cd runtime-dirs/databrary-unit-impure && cabal -j test --test-options="--color always --hide-successes --timeout 2s"'
