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
in {inherit postgres pgranges; }
