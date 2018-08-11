#!/usr/bin/env bash
set -e
# jsCssVersion.txt and empty web directory are still needed to start the backend
# TODO: make flag to start backend only
nix-shell --attr databrary-dev.env default.nix --command 'cd runtime-dirs/databrary-unit-impure && ../../ghci-databrary.sh --skip-ghci'
nix-shell --attr databrary-dev.env default.nix --command 'cabal configure --enable-tests --datadir=. --datasubdir=. && cabal build discovered && cd runtime-dirs/databrary-unit-impure && ../../dist/build/discovered/discovered --color always --hide-successes --timeout 2s"'
