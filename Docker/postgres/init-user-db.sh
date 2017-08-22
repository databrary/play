#!/usr/bin/env bash
set -e
PGPASSWORD=mysecretpassword psql -v ON_ERROR_STOP=1 --username "postgres" <<-EOSQL
    CREATE USER databrary;
    CREATE DATABASE databrary;
    GRANT ALL PRIVILEGES ON DATABASE databrary TO databrary;
    ALTER USER databrary WITH PASSWORD 'databrary123';
    ALTER USER databrary WITH SUPERUSER;
EOSQL

