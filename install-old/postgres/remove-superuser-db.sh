#!/bin/bash
set -e
psql -v ON_ERROR_STOP=1 --username "postgres" <<-EOSQL
    ALTER USER databrary WITH NOSUPERUSER;
EOSQL

