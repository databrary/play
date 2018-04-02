{ mkDerivation, aeson, aeson-better-errors, array, attoparsec, base
, bcrypt, binary, blaze-html, blaze-markup, bytestring
, case-insensitive, cassava, conduit-combinators, containers, cookie, cracklib
, cryptonite, data-default-class, directory, fast-logger, ffmpeg
, filepath, hashable, hjsonschema, http-client, http-client-tls
, http-types, invertible
, lifted-base, memory, mime-mail, mime-types, monad-control, mtl
, network, network-uri, openssl, parsec, path, path-io, posix-paths
, postgresql-simple, postgresql-typed, process, range-set-list
, regex-posix, resource-pool, resourcet, scientific, servant, servant-server, smtp-mail
, stdenv, streaming-commons, stringsearch, tasty, tasty-expected-failure
, tasty-hunit, template-haskell, text, th-lift, th-lift-instances
, time, transformers, transformers-base, unix, unordered-containers
, utf8-string, vector, wai, wai-extra, wai-route, warp, warp-tls
, web-inv-route, xml, zip, zlib
, gargoyle, gargoyle-postgresql, postgresql
, nodePackages, nodejs, dbName ? "databrary-nix-db", jdk
, cpio, coreutils
}:
let
  # Override the drv with these attrs. (Can't put them in mkDerivation directly
  # because the Haskell-overridden function doesn't accept them.)
  attrOverrides = {
    # FIXME:kanishka what did this solve?
    dontPatchELF = true;
    # was only needed when trans scripts were being copied by data-files
    dontPatchShebangs = true;
  };
in
(mkDerivation rec {
  pname = "databrary";
  doHaddock = false;
  version = "1";
  src =
    builtins.filterSource
      (path: _type: ! builtins.elem
          (baseNameOf path)
          [dbName ".git" "result" "node_modules"])
      ./.;
  isLibrary = true;
  enableStaticLibraries = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-better-errors array attoparsec base bcrypt binary
    blaze-html blaze-markup bytestring case-insensitive cassava
    conduit-combinators containers cookie cryptonite data-default-class
    directory fast-logger filepath hashable hjsonschema http-client
    http-client-tls http-types invertible lifted-base memory mime-mail
    mime-types monad-control mtl network network-uri parsec path
    path-io posix-paths postgresql-simple postgresql-typed process
    range-set-list regex-posix resource-pool resourcet scientific
    servant servant-server smtp-mail streaming-commons stringsearch template-haskell text th-lift
    th-lift-instances time transformers transformers-base unix
    unordered-containers utf8-string vector wai wai-extra wai-route warp warp-tls
    web-inv-route xml zip zlib
    gargoyle gargoyle-postgresql
  ];
  librarySystemDepends = [ cracklib openssl openssl.dev ];
  libraryPkgconfigDepends = [
    ffmpeg
  ];
  executableHaskellDepends = [
    aeson aeson-better-errors array attoparsec base bcrypt binary
    blaze-html blaze-markup bytestring case-insensitive cassava
    conduit-combinators containers cookie cryptonite data-default-class
    directory fast-logger filepath hashable hjsonschema http-client
    http-client-tls http-types invertible lifted-base memory mime-mail
    mime-types monad-control mtl network network-uri parsec path
    path-io posix-paths postgresql-simple postgresql-typed process
    range-set-list regex-posix resource-pool resourcet scientific
    servant servant-server smtp-mail streaming-commons stringsearch template-haskell text th-lift
    th-lift-instances time transformers transformers-base unix
    unordered-containers utf8-string vector wai wai-extra wai-route warp warp-tls
    web-inv-route xml zip zlib
    gargoyle gargoyle-postgresql
  ];
  executableSystemDepends = [ cracklib openssl openssl.dev ];
  executablePkgconfigDepends = [
    ffmpeg
  ];
  testHaskellDepends = [
    base tasty tasty-expected-failure tasty-hunit
  ];
  executableToolDepends = [
    postgresql
    # Put coffee, uglifyjs, etc in scope
    nodePackages.shell.nodeDependencies
    nodejs
    jdk
    coreutils # cat, cut, md5sum
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
    # Link node_modules into the current directory
    ln -sf ${nodePackages.shell.nodeDependencies}/lib/node_modules
  '';
  postInstall = ''
    databrary_datadir=. $out/bin/databrary -w
  '';
  postFixup = ''
    data_basedir="$out/share/x86_64-linux-ghc-8.0.2"
    data_outputdir="$data_basedir/databrary-1"
    mkdir -p $data_outputdir

    cp messages.conf volume.json $data_outputdir

    cp jsCssVersion.txt $data_outputdir
    mkdir $data_outputdir/web
    cd web
    cp all.min.css all.min.css.gz all.min.js all.min.js.gz constants.json constants.json.gz $data_outputdir/web
    cp -r icons images $data_outputdir/web
    cd - > /dev/null

    cp transcode transctl.sh $data_outputdir

    # repeated in ghci databrary
    cp install/cracklib-dicts-2.9.0-11.el7.x86_64.cpio /tmp
    cd /tmp
    ${cpio}/bin/cpio -idmv < cracklib-dicts-2.9.0-11.el7.x86_64.cpio
    cd -
    mkdir -p $data_outputdir/cracklib
    cp -r /tmp/usr/share/cracklib/pw_dict* $data_outputdir/cracklib

    mkdir $data_outputdir/solr
    cp -r solr/solr.xml solr/log4j.properties solr/conf $data_outputdir/solr

    cp -r schema $data_outputdir
  '';
}).overrideAttrs (_oldAttrs: attrOverrides)
