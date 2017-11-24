#!/usr/bin/env bash
./init-db &
sleep 7
PGUSER=postgres PGHOST=$(readlink -f databrary-nix-db/work/) psql
kill $(head -1 databrary-nix-db/work/postmaster.pid)
sleep 2
