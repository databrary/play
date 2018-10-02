{ coverage ? false
, haddock ? false
}:

let
  reflex-platform = import
    ((import <nixpkgs> {}).fetchFromGitHub {
      owner= "reflex-frp";
      repo = "reflex-platform";
      rev = "bdc94c605bf72f1a65cbd12075fbb661e28b24ea";
      sha256 = "1i4zk7xc2x8yj9ms4gsg70immm29dp8vzqq7gdzxig5i3kva0a61";
  }) {};
  # Definition of nixpkgs, version controlled by Reflex-FRP
  nixpkgs = reflex-platform.nixpkgs;
  inherit (nixpkgs.lib) id;
  nodePackages = import ./node-default.nix { pkgs = nixpkgs; };
  inherit (nixpkgs) fetchFromGitHub writeScriptBin cpio wget;
  # nixpkgs functions used to regulate Haskell overrides
  inherit (nixpkgs.haskell.lib)
    dontCheck overrideCabal doJailbreak doCoverage doHaddock dontHaddock
    disableSharedLibraries disableSharedExecutables;
  noSharedObjs = x: disableSharedExecutables (disableSharedLibraries x);
  # Pin at postgresql95, which is what's in prod
  postgresql = nixpkgs.postgresql95;
  gargoyleSrc = fetchFromGitHub {
        owner= "obsidiansystems";
        repo = "gargoyle";
        rev = "87039dac83a8899a6c66fa681e6e77140b3ddacc";
        sha256 = "04xr8bl9mfcv0lmbb4y8ach7h44qbiyq925wjcl5x039bmz24f4k";
  };
  inherit (nixpkgs) coreutils;
  # Define GHC compiler override
  pkgs = reflex-platform.ghc.override {
    overrides = self: super: rec {
      databrary =
        (if haddock then doHaddock else dontHaddock)
          ((if coverage then doCoverage else id)
            (noSharedObjs
              (self.callPackage ./databrary.nix {
                inherit postgresql nodePackages coreutils;
                # ffmpeg override with with --enable-libfdk-aac and --enable-nonfree flags set
                ffmpeg = nixpkgs.ffmpeg-full.override {
                  nonfreeLicensing = true;
                  fdkaacExtlib = true;
                };
        })));
      # cabal override to enable ghcid (GHCi daemon) development tool
      databrary-dev = overrideCabal databrary (drv: {
        libraryHaskellDepends =
          (drv.libraryHaskellDepends or [])
           ++
             (with self;
               [ghcid cabal-install
               # for ghci-databrary script
               wget cpio nodePackages.shell.nodeDependencies]);
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
      # Define zip with special fork including streaming support; dontCheck to save time
      zip = dontCheck (self.callPackage ./zip-blind.nix {});
    };
  };
in { databrary = pkgs.databrary; databrary-dev = pkgs.databrary-dev; }
