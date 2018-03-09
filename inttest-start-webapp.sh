#!/bin/bash
cat reload-gen-start | nix-shell --attr databrary-dev.env default.nix --command "ghci-databrary"
