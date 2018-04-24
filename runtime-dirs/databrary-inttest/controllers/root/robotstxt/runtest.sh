#!/usr/bin/env bash
set -e

git_root_dir=$(git rev-parse --show-toplevel)
nix-shell --attr databrary-dev.env $git_root_dir/default.nix --command "runghc fetchresult.hs | runghc checkresult.hs"
