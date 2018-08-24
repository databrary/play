let
    pkgs' = import ./nixpkgs.nix;

    hlib = pkgs'.haskell.lib;

    haskellOverrides = hsSelf: hsSuper: {
        #################
        # Customized dep versions.
        ################
        #
        # FIXME: Describe why we are stuck on these old versions.

        # specific version: 0.5 breaks things, I think
        #
        # dontCheck: The test assumes a postgres socket at /tmp/.s.PGSQL, and
        # this package is totally bogus anyway.
        postgresql-typed =
                hlib.dontCheck
                    (hsSelf.callHackage "postgresql-typed" "0.4.5" {});

        # Need a version <= 0.10 for postgresql-typed-0.4.5
        #
        # FIXME: dontCheck: wants a postgres service available (this is doable,
        # but I can't be bothered right now).
        postgresql-binary = hlib.dontCheck
            (hsSelf.callHackage  "postgresql-binary" "0.10" {});

        # FIXME: deprecated
        # test fails because of missing file (?)
        hjsonschema = hlib.dontCheck
            (hlib.doJailbreak (hsSelf.callHackage "hjsonschema" "0.9.0.0" {}));

        # hjsonpointer-0.3.0.2 needed for hjsonschema-0.9.0.0
        hjsonpointer =  hlib.doJailbreak (hsSelf.callHackage "hjsonpointer" "0.3.0.2" {});


        ################
        # Local packages
        ################
        databrary = hsSelf.callCabal2nix "databrary" ./backend {
            # TODO: This should be specified as an overlay, but our version of nixpkgs (aka
            # reflex-platform) doesn't support them.
            ffmpeg = pkgs'.ffmpeg-full.override {
                nonfreeLicensing = true;
                fdkaacExtlib = true;
            };
            # This is supplied by the ffmpeg package.
            libavcodec = null;
        };
    };

    pkgs = {
        haskellPackages = pkgs'.haskellPackages.override {
            overrides = haskellOverrides;
        };
    };

in
    pkgs.haskellPackages.databrary
