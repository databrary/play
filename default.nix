{ nodePackages ? import ./node-default.nix {}
, databraryRoot ? ./.
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
  inherit (nixpkgs) fetchFromGitHub writeScriptBin cpio wget;
  # nixpkgs functions used to regulate Haskell overrides
  inherit (nixpkgs.haskell.lib) dontCheck overrideCabal doJailbreak;
  ghciDatabrary = writeScriptBin "ghci-databrary" ''
    if [ ! -d "solr-6.6.0" ]; then
      if [ ! -d "/tmp/solr-6.6.0" ]; then
        pushd /tmp > /dev/null
        ${wget}/bin/wget -qO- http://archive.apache.org/dist/lucene/solr/6.6.0/solr-6.6.0.tgz | tar -zxv
        popd > /dev/null
      fi
      cp -R /tmp/solr-6.6.0 .
    fi
    if [ ! -d "cracklib" ]; then
      echo download and create cracklib dict
      # {wget}/bin/wget http://mirror.centos.org/centos/7/os/x86_64/Packages/cracklib-dicts-2.9.0-11.el7.x86_64.rpm
      # {rpm}/bin/rpm2cpio cracklib-dicts-2.9.0-11.el7.x86_64.rpm > tmp/cracklib-dicts-2.9.0-11.el7.x86_64.cpio
      cp install/cracklib-dicts-2.9.0-11.el7.x86_64.cpio /tmp
      cd /tmp
      ${cpio}/bin/cpio -idmv < cracklib-dicts-2.9.0-11.el7.x86_64.cpio
      cd -
      mkdir -p cracklib
      cp -r /tmp/usr/share/cracklib/pw_dict* cracklib
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
    if [ ! -e "config/email" ]; then
      cp install/config.email config/email
    fi
    rm -rf dist
    cabal configure --datadir=. --datasubdir=.
    cabal repl exe:databrary
  '';
  postgresql = import ./db.nix { inherit nixpkgs; };
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
      databrary = self.callPackage ./databrary.nix {
        # postgresql with ranges plugin
        inherit postgresql nodePackages coreutils;
        # ffmpeg override with with --enable-libfdk-aac and --enable-nonfree flags set
        ffmpeg = nixpkgs.ffmpeg-full.override {
          nonfreeLicensing = true;
          fdkaacExtlib = true;
        };
      };
      # cabal override to enable ghcid (GHCi daemon) development tool
      databrary-dev = overrideCabal databrary (drv: {
        libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (with self; [ghcid cabal-install ghciDatabrary]);
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
