#!/usr/bin/env bash
set -eu

nix-shell -p entr --command "find web | entr ./generate.sh"
