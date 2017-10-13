{ reflex-platform ? import ./reflex-platform {}
, nodePackages ? import ./node-default.nix {}
, databraryRoot ? ./.
, conf ? import ./conf.nix { inherit databraryRoot; }
}:

let
  # Definition of nixpkgs, version controlled by Reflex-FRP
	nixpkgs = reflex-platform.nixpkgs;
  # nixpkgs functions used to regulate Haskell overrides
  dontCheck = nixpkgs.haskell.lib.dontCheck;
  overrideCabal = nixpkgs.haskell.lib.overrideCabal;
	doJailbreak = nixpkgs.haskell.lib.doJailbreak;
  postgresql = import ./db.nix { inherit reflex-platform; };

  # Define GHC compiler override
  pkgs = reflex-platform.ghc.override {

    overrides = self: super: {
      databrary = self.callPackage ./databrary.nix {
        # postgresql with ranges plugin
        inherit postgresql;
        # ffmpeg override with with --enable-libfdk-aac and --enable-nonfree flags set
        ffmpeg = nixpkgs.ffmpeg-full.override {
          nonfreeLicensing = true;
          fdkaacExtlib = true;
        };
      };
      
      # cabal override to enable ghcid (GHCi daemon) development tool
      databrary-dev = overrideCabal self.databrary (drv: {
        libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ [self.ghcid];
      });

      # Define postgresql-typed package with explicit version number
    	postgresql-typed = dontCheck (self.callHackage  "postgresql-typed" "0.4.5" {});
    	postgresql-binary = dontCheck (self.callHackage  "postgresql-binary" "0.10" {});
			
			#partial-isomorphisms is for GHC7 only!
			# partial-isomorphisms= dontCheck (self.callHackage  "partial-isomorphisms"
      #"0.2" {});

      # Define hjsonschema  package with explicit version number
			hjsonschema = dontCheck (doJailbreak (self.callHackage "hjsonschema" "0.9.0.0" {}));	
      # Define hjsonpointer  package with explicit version number
			hjsonpointer = dontCheck (doJailbreak (self.callHackage "hjsonpointer" "0.3.0.2" {}));
      # Define invertible as invertible from reflex-platform 
		 	invertible = dontCheck super.invertible;

      gargoyle = dontCheck (self.callHackage "gargoyle" "0.1" {}); 
    };
  };
in { inherit nixpkgs pkgs nodePackages conf; }
