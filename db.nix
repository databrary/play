{ reflex-platform ? import ./reflex-platform {}
}:
let
  nixpkgs = reflex-platform.nixpkgs;
  postgres = nixpkgs.postgresql96;
in
nixpkgs.writeScript "run-webdriver-tests" ''
  ${postgres}/bin/initdb -D databrary-db
  ${postgres}/bin/pg_ctl start -w -D databrary-db
  ${postgres}/bin/createuser -s postgres
  PGPASSWORD=mysecretpassword ${postgres}/bin/psql -v ON_ERROR_STOP=1 --username "postgres" <<-EOSQL
      CREATE USER databrary;
      CREATE DATABASE databrary;
      GRANT ALL PRIVILEGES ON DATABASE databrary TO databrary;
      ALTER USER databrary WITH PASSWORD 'databrary123';
      ALTER USER databrary WITH SUPERUSER;
  EOSQL

  for file in ./schema/*
  do
    ${postgres}/bin/psql databrary < "$file"
  done
''
