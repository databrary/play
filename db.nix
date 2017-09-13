{ reflex-platform ? import ./reflex-platform {}
, dbPath ? "databrary-db"
}:
let
  nixpkgs = reflex-platform.nixpkgs;
  pg = nixpkgs.postgresql96;
  pgranges = nixpkgs.callPackage ./pgranges {};

  postgres = nixpkgs.buildEnv {
    name = "postgres96withRanges";
    paths = [ pg pg.lib pgranges ];
    buildInputs = [ nixpkgs.makeWrapper ];
    postBuild = ''
      mv $out/bin $out/old_bin
      mkdir $out/bin
      cp --target-directory=$out/bin $out/old_bin/*
      rm $out/old_bin

      wrapProgram $out/bin/postgres --set NIX_PGLIBDIR $out/lib
    '';
  };
in
nixpkgs.writeScript "run-webdriver-tests" ''
  if [ -d ${dbPath} ]; then
     # Start the db if it's not running.
     set +e
     ${postgres}/bin/pg_ctl status -D "${dbPath}"
     if [ $? -eq 3 ]; then
       ${postgres}/bin/pg_ctl start -w -D "${dbPath}"
     fi
     set -e
  else
    ${postgres}/bin/initdb -D ${dbPath}
    ${postgres}/bin/pg_ctl start -w -D ${dbPath}
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
  fi
''
