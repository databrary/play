{ stdenv, postgresql96 }:

stdenv.mkDerivation {
  name = "pgranges"; 
  src = ./.;
  dontBuild = true;
  nativeBuildInputs = [ postgresql96 ]; 
  preInstall = '' 
    export NIX_PGLIBDIR=$out/lib
  '';
}
