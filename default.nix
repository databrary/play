{ nodePackages ? import ./node-default.nix {}
, databraryRoot ? ./.
}:

let
  reflex-platform = import
    ((import <nixpkgs> {}).fetchFromGitHub {
      owner= "reflex-frp";
      repo = "reflex-platform";
      rev = "08cff0f6724767724592c6649c249d3c83d0eea8";
      sha256 = "02jjhfwx41q7a8kivic05d7mgbani4z8ww9db9flyz13vgx240b0";
    }) {};
  # Definition of nixpkgs, version controlled by Reflex-FRP
	nixpkgs = reflex-platform.nixpkgs;
  fetchFromGitHub = nixpkgs.fetchFromGitHub;
  # nixpkgs functions used to regulate Haskell overrides
  dontCheck = nixpkgs.haskell.lib.dontCheck;
  overrideCabal = nixpkgs.haskell.lib.overrideCabal;
	doJailbreak = nixpkgs.haskell.lib.doJailbreak;
  postgresql = import ./db.nix { inherit nixpkgs; };
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
      gargoyle = self.callPackage "${gargoyleSrc}/gargoyle" {};
      gargoyle-postgresql= self.callPackage "${gargoyleSrc}/gargoyle-postgresql" {};
      # Define hjsonschema  package with explicit version number
			hjsonschema = dontCheck (doJailbreak (self.callHackage "hjsonschema" "0.9.0.0" {}));
      # Define hjsonpointer  package with explicit version number
			hjsonpointer = dontCheck (doJailbreak (self.callHackage "hjsonpointer" "0.3.0.2" {}));
      # Define invertible as invertible from reflex-platform 
		 	invertible = dontCheck super.invertible;
      # postgresql-typed 0.4.5 requires a version <= 0.10
      postgresql-binary = dontCheck (self.callHackage  "postgresql-binary" "0.10" {});
      # Define postgresql-typed package with explicit version number
      postgresql-typed = dontCheck (self.callHackage  "postgresql-typed" "0.4.5" {});
    };
  };
in { inherit nixpkgs pkgs nodePackages postgresql; }
