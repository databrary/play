{ mkDerivation, base, bytestring, bzlib-conduit, case-insensitive
, cereal, conduit, conduit-extra, containers, criterion, digest
, exceptions, fetchgit, filepath, hspec, mtl, path, path-io, plan-b
, QuickCheck, resourcet, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "zip";
  version = "0.1.3";
  src = fetchgit {
    url = "http://github.com/robertleegdm/zip.git";
    sha256 = "1kbkd67rjvnhxp3p18pqbdv50vxr4k5d1mziyf7hhv5nkpv4g32c";
    rev = "594d57a09f6b957ba0fb54151f49e10a2eba4fdd";
  };
  libraryHaskellDepends = [
    base bytestring bzlib-conduit case-insensitive cereal conduit
    conduit-extra containers digest exceptions filepath mtl path
    path-io plan-b resourcet text time transformers
  ];
  testHaskellDepends = [
    base bytestring conduit containers exceptions filepath hspec path
    path-io QuickCheck text time transformers
  ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://github.com/mrkkrp/zip";
  description = "Operations on zip archives";
  license = stdenv.lib.licenses.bsd3;
}
