#!/bin/bash
nix-shell --attr databrary-dev.env default.nix --command "./init-db"
