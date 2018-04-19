#!/usr/bin/env bash

nix-shell --attr databrary-dev.env default.nix --command "cd runtime-dirs/databrary-inttest && ./init-db"
