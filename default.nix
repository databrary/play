{ reflex-platform ? import ./reflex-platform {}
, nodePackages ? import ./node-default.nix {}
, databraryRoot ? ./.
, conf ? import ./conf.nix { inherit databraryRoot; }
}:

let
  # Definition of nixpkgs, version controlled by Reflex-FRP
	nixpkgs = reflex-platform.nixpkgs;
  fetchFromGitHub = nixpkgs.fetchFromGitHub;
  # nixpkgs functions used to regulate Haskell overrides
  dontCheck = nixpkgs.haskell.lib.dontCheck;
  overrideCabal = nixpkgs.haskell.lib.overrideCabal;
	doJailbreak = nixpkgs.haskell.lib.doJailbreak;
  postgresql = import ./db.nix { inherit reflex-platform; };
  gargoyleSrc = fetchFromGitHub {
        owner= "obsidiansystems";
        repo = "gargoyle";
        rev = "87039dac83a8899a6c66fa681e6e77140b3ddacc";
        sha256 = "04xr8bl9mfcv0lmbb4y8ach7h44qbiyq925wjcl5x039bmz24f4k";
  };

  # Define GHC compiler override
  pkgs = reflex-platform.ghc.override {

    overrides = self: super: rec {
      databrary = self.callPackage ./databrary.nix {
        # postgresql with ranges plugin
        inherit postgresql nodePackages;
        # ffmpeg override with with --enable-libfdk-aac and --enable-nonfree flags set
        ffmpeg = nixpkgs.ffmpeg-full.override {
          nonfreeLicensing = true;
          fdkaacExtlib = true;
        };
      };
      
      # cabal override to enable ghcid (GHCi daemon) development tool
      databrary-dev = overrideCabal databrary (drv: {
        libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ [self.ghcid];
      });

      # Define postgresql-typed package with explicit version number
    	postgresql-typed = dontCheck (self.callHackage  "postgresql-typed" "0.4.5" {});
			
			#partial-isomorphisms is for GHC7 only!
			# partial-isomorphisms= dontCheck (self.callHackage  "partial-isomorphisms"
      #"0.2" {});

      # Define hjsonschema  package with explicit version number
			hjsonschema = dontCheck (doJailbreak (self.callHackage "hjsonschema" "0.9.0.0" {}));	
      # Define hjsonpointer  package with explicit version number
			hjsonpointer = dontCheck (doJailbreak (self.callHackage "hjsonpointer" "0.3.0.2" {}));
      # Define invertible as invertible from reflex-platform 
		 	invertible = dontCheck super.invertible;

      gargoyle = self.callPackage "${gargoyleSrc}/gargoyle" {} ;
      gargoyle-postgresql= self.callPackage "${gargoyleSrc}/gargoyle-postgresql" {} ;

    };
  };
in { inherit nixpkgs pkgs nodePackages conf postgresql; }
