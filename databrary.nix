{ mkDerivation, aeson, aeson-better-errors, array, attoparsec, base
, bcrypt, blaze-builder, blaze-html, blaze-markup, bytestring
, case-insensitive, containers, cookie, cracklib, cryptonite
, data-default-class, digest, directory, fast-logger, ffmpeg
, file-embed, filepath, hashable, haskell-src-meta, hjsonschema
, http-client, http-client-tls, http-types, invertible
, lifted-base, memory, mime-mail, postgresql
, monad-control, mtl, network, network-uri, parsec, posix-paths
, postgresql-typed, process, range-set-list, regex-posix
, resource-pool, resourcet, scientific, smtp-mail, stdenv
, streaming-commons, template-haskell, text, th-lift
, th-lift-instances, time, transformers, transformers-base, unix
, unordered-containers, utf8-string, vector, wai, wai-extra, warp
, warp-tls, web-inv-route, xml, zlib, gargoyle, gargoyle-postgresql, postgresql-simple, postgresql-simple-url
}:
mkDerivation rec {
  pname = "databrary";
  doCheck = false;
  version = "1";
  src = builtins.filterSource 
    (path: type: type == "directory" || baseNameOf path != ".git" || baseNameOf path == ".cabal")
    ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-better-errors array attoparsec base bcrypt
    blaze-builder blaze-html blaze-markup bytestring case-insensitive
    containers cookie cryptonite data-default-class digest directory
    fast-logger file-embed filepath hashable haskell-src-meta
    hjsonschema http-client http-client-tls http-types invertible
    lifted-base memory mime-mail monad-control mtl network network-uri
    parsec posix-paths postgresql-typed process range-set-list
    regex-posix resource-pool resourcet scientific smtp-mail
    streaming-commons template-haskell text th-lift th-lift-instances
    time transformers transformers-base unix unordered-containers
    utf8-string vector wai wai-extra warp warp-tls web-inv-route xml
    zlib gargoyle gargoyle-postgresql postgresql-simple postgresql-simple-url
  ];
  executableSystemDepends = [ cracklib ];
  executablePkgconfigDepends = [
    ffmpeg
  ];
  executableToolDepends = [
    postgresql.pg
  ];
  description = "Databrary";
  license = stdenv.lib.licenses.gpl3;
  # dbPath = "${./.}" ++ "databrary-local-db";
  # dbPath = "databrary-local-db";
  preBuild = '' 
    set -x
    # set -v
    mkdir -v -p $out
    # mkdir -v -p "$out/databrary-local-db"
    echo $out
    ${gargoyle-postgresql}/bin/gargoyle-psql $out/databrary-local-db <<-EOSQL
     CREATE USER databrary;
     CREATE DATABASE databrary;
     GRANT ALL PRIVILEGES ON DATABASE databrary TO databrary;
     ALTER USER databrary WITH PASSWORD 'databrary123';
     ALTER USER databrary WITH SUPERUSER;
EOSQL

    for file in ./schema/*
    do
      ${gargoyle-postgresql}/bin/gargoyle-psql "$out/databrary-local-db" < "$file"
    done
    
    ${gargoyle-postgresql}/bin/gargoyle-psql "$out/databrary-local-db" & 
  '';
}
