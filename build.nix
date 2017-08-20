{ reflex-platform ? import ./reflex-platform {}
, nodePackages ? import ./node-default.nix {}
, databraryRoot ? ./.
, conf ? import ./conf.nix { inherit databraryRoot; }
}:

let
	nixpkgs = reflex-platform.nixpkgs;
  dontCheck = nixpkgs.haskell.lib.dontCheck;
  overrideCabal = nixpkgs.haskell.lib.overrideCabal;
	doJailbreak = nixpkgs.haskell.lib.doJailbreak;

  pkgs = reflex-platform.ghc.override {

  # reflex-platform.ghc7 fails with infinite recursion
  # 
	# pkgs =	nixpkgs.pkgs.haskell.packages.ghc7103.override {
    overrides = self: super: rec {
      databrary = self.callPackage ./. {inherit (nixpkgs) ffmpeg nodejs;};
      
      databrary-dev = overrideCabal databrary (drv: {
        libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ [self.ghcid];
      });

      # nix version is 0.5.1
    	# postgresql-typed = dontCheck super.postgresql-typed; 
      # databrary is using 0.4.5
    	postgresql-typed = dontCheck (self.callHackage  "postgresql-typed" "0.4.5" {});
			
			#partial-isomorphisms is for GHC7 only!
			# partial-isomorphisms= dontCheck (self.callHackage  "partial-isomorphisms"
      #"0.2" {});

			hjsonschema = dontCheck (doJailbreak (self.callHackage "hjsonschema" "0.9.0.0" {}));	
			hjsonpointer = dontCheck (doJailbreak (self.callHackage "hjsonpointer" "0.3.0.2" {}));
		 	invertible = dontCheck super.invertible;
    };
  };
in { inherit nixpkgs pkgs nodePackages conf; }
