{ mkDerivation, aeson, aeson-better-errors, array, attoparsec, base
, bcrypt, blaze-builder, blaze-html, blaze-markup, bytestring
, case-insensitive, containers, cookie, cracklib, cryptonite
, data-default-class, digest, directory, fast-logger, ffmpeg
, file-embed, filepath, hashable, haskell-src-meta, hjsonschema
, http-client, http-client-tls, http-types, invertible
, lifted-base, memory, mime-mail, mime-types, postgresql
, monad-control, mtl, network, network-uri, parsec, posix-paths
, postgresql-typed, process, range-set-list, regex-posix
, resource-pool, resourcet, scientific, smtp-mail, stdenv
, streaming-commons, template-haskell, text, th-lift
, th-lift-instances, time, transformers, transformers-base, unix
, unordered-containers, utf8-string, vector, wai, wai-extra, warp
, warp-tls, web-inv-route, xml, zlib, gargoyle, gargoyle-postgresql, postgresql-simple, postgresql-simple-url
, nodePackages, nodejs, openssl, dbName ? "databrary-nix-db", jdk
}:
mkDerivation rec {
  pname = "databrary";
  doCheck = false;
  version = "1";
  src = builtins.filterSource 
    (path: type: type == "directory" || baseNameOf path != ".git" || baseNameOf path == ".cabal" || path != dbName)
    ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-better-errors array attoparsec base bcrypt
    blaze-builder blaze-html blaze-markup bytestring case-insensitive
    containers cookie cryptonite data-default-class digest directory
    fast-logger file-embed filepath hashable haskell-src-meta
    hjsonschema http-client http-client-tls http-types invertible
    lifted-base memory mime-mail mime-types monad-control mtl network
    network-uri parsec posix-paths postgresql-typed process range-set-list
    regex-posix resource-pool resourcet scientific smtp-mail
    streaming-commons template-haskell text th-lift th-lift-instances
    time transformers transformers-base unix unordered-containers
    utf8-string vector wai wai-extra warp warp-tls web-inv-route xml
    zlib gargoyle gargoyle-postgresql postgresql-simple postgresql-simple-url
  ];
  executableSystemDepends = [ cracklib openssl openssl.dev ];
  executablePkgconfigDepends = [
    ffmpeg
  ];
  executableToolDepends = [
    postgresql.postgres
    # Put coffee, uglifyjs, etc in scope
    nodePackages.shell.nodeDependencies
    nodejs
    jdk
  ];
  description = "Databrary";
  license = stdenv.lib.licenses.gpl3;
  preBuild = ''
    set -x
    ./initializeDB.sh
    # ensure gargoyle-psql had enough time to shudown postgres
    sleep 5

    socket_path=$(pwd)/${dbName}/work/
    echo "$socket_path"

    postgres -D $socket_path -k . -h "" &

    cat databrary.conf
    ls -la $(dirname $socket_path)
    ls -la $socket_path
  '';
  postBuild = ''
    kill -INT `head -1 $socket_path/postmaster.pid`
    ln -s ${nodePackages.shell.nodeDependencies}/lib/node_modules node_modules
    databrary_datadir=. dist/build/databrary/databrary -w
  '';
  postInstall = '' 
    chmod +x $out/share/x86_64-linux-ghc-8.0.2/databrary-1/transctl.sh
    chmod +x $out/share/x86_64-linux-ghc-8.0.2/databrary-1/transcode
    cp cracklib $out/share/x86_64-linux-ghc-8.0.2/databrary-1/
  '';
}
