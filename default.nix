{ mkDerivation, aeson, aeson-better-errors, array, attoparsec, base
, bcrypt, blaze-builder, blaze-html, blaze-markup, bytestring
, case-insensitive, containers, cookie, cracklib, cryptonite
, data-default-class, digest, directory, fast-logger, ffmpeg
, file-embed, filepath, hashable, haskell-src-meta, hjsonschema
, http-client, http-client-tls, http-types, invertible
, lifted-base, memory, mime-mail
, monad-control, mtl, network, network-uri, parsec, posix-paths
, postgresql-typed, process, range-set-list, regex-posix
, resource-pool, resourcet, scientific, smtp-mail, stdenv
, streaming-commons, template-haskell, text, th-lift
, th-lift-instances, time, transformers, transformers-base, unix
, unordered-containers, utf8-string, vector, wai, wai-extra, warp
, warp-tls, web-inv-route, xml, zlib
}:
mkDerivation {
  pname = "databrary";
  doCheck = false;
  version = "1";
  src = ./.;
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
    zlib
  ];
  executableSystemDepends = [ cracklib ];
  executablePkgconfigDepends = [
    ffmpeg
  ];
  description = "Databrary";
  license = stdenv.lib.licenses.gpl3;
}
