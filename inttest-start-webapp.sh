#!/bin/bash
cat reload-gen-start-inttest | nix-shell --attr databrary-dev.env default.nix --command "ghci-databrary"

