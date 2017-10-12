{ reflex-platform ? import ./reflex-platform {}
, dbPath ? "databrary-db"
}:
let
  # Definition of nixpkgs, version controlled by Reflex-FRP
  nixpkgs = reflex-platform.nixpkgs;
  # PostgreSQL 9.6
  pg = nixpkgs.postgresql96;
  # Posgres ranges extension
  pgranges = nixpkgs.callPackage ./pgranges {};

  # Custom Postgresql environment
  # set PGLIBDIR
in
  nixpkgs.buildEnv {
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
  }

