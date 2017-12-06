#!/usr/bin/env bash
set -eu

# this does not use shell.nix or any databrary specific dependencies yet
nix-shell --attr pkgs.databrary-dev.env --run "find web | entr ./generate.sh" default.nix
