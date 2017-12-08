{ nodePackages ? import ./node-default.nix {}
, databraryRoot ? ./.
}:

let
  reflex-platform = import
    ((import <nixpkgs> {}).fetchFromGitHub {
      owner= "reflex-frp";
      repo = "reflex-platform";
      rev = "2884a9abb9eec7fdbc7373cafcb2d42bfdf1f67a";
      sha256 = "0qnmxbpxzkiqp477d2msy93gv4cm4npcxs2cyvbpkj4x1b5j08m2";
  }) {};
  # Definition of nixpkgs, version controlled by Reflex-FRP
  nixpkgs = reflex-platform.nixpkgs;
  inherit (nixpkgs) fetchFromGitHub writeScriptBin;
  # nixpkgs functions used to regulate Haskell overrides
  inherit (nixpkgs.haskell.lib) dontCheck overrideCabal doJailbreak;
  ghciDatabrary = writeScriptBin "ghci-databrary" ''
    if [ ! -d "solr-6.6.0" ]; then
      wget -qO- http://archive.apache.org/dist/lucene/solr/6.6.0/solr-6.6.0.tgz | tar -zxv
    fi
    if [ ! -d "node_modules" ]; then
      echo linking node_modules
      ln -s ${nodePackages.shell.nodeDependencies}/lib/node_modules node_modules
    fi
    # make store related dirs
    mkdir -p cache/tmp stage tmp trans upload
    if [ ! -d "store" ]; then
      cp -R install/store-seed store
    fi
    if [ ! -d "databrary_logs" ]; then
      mkdir databrary_logs
      touch databrary_logs/solr_log
    fi
    rm -rf dist
    cabal configure --datadir=. --datasubdir=.
    cabal repl databrary
  '';
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
        libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (with self; [ghcid cabal-install ghciDatabrary nixpkgs.wget]);
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
in { databrary = pkgs.databrary; databrary-dev = pkgs.databrary-dev; }
