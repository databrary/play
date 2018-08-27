let
    pkgs' = import ./nixpkgs.nix;
    hlib = pkgs'.haskell.lib;

    dbMakefile = ./db.makefile;

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
        hjsonpointer = hlib.doJailbreak
            (hsSelf.callHackage "hjsonpointer" "0.3.0.2" {});

        # Customized blind streaming zip library. FIXME: Upstream this
        # WARNING: This package's tests take 5-10 minutes.
        zip = hsSelf.callCabal2nix "zip"
            (pkgs'.fetchFromGitHub
                {
                    owner = "robertleegdm";
                    repo = "zip";
                    rev = "594d57a09f6b957ba0fb54151f49e10a2eba4fdd";
                    sha256 = "1kbkd67rjvnhxp3p18pqbdv50vxr4k5d1mziyf7hhv5nkpv4g32c";
                }
            )
            {};

        ################
        # Local packages
        ################
        databrary = hlib.dontHaddock (
            (hsSelf.callCabal2nix
                "databrary"
                ./backend
                {
                    # TODO: This should be specified as an overlay, but our
                    # version of nixpkgs (aka reflex-platform) doesn't support
                    # them.
                    ffmpeg = pkgs'.ffmpeg-full.override
                        {
                            nonfreeLicensing = true;
                            fdkaacExtlib = true;
                        };
                    # These are supplied by the ffmpeg package.
                    libavcodec = null;
                    libavformat = null;
                    libswscale = null;

                    # Name clash. We want the system package, not the haskell
                    # package.
                    crack = pkgs'.cracklib;
                }
            ).overrideAttrs (_:
                {
                    preBuild = ''
                        export PGHOST=$(pwd)/.postgres-work
                        export PGDATABASE=default
                        export PGUSER=$(whoami)
                        make -f ${dbMakefile}
                    '';
                    postCheck = ''
                        make -f ${dbMakefile} stop
                    '';
                    # FIXME: Untested. Can't remember off the top of my head if
                    # this is the right attribute to set.
                    failureHook = ''
                        make -f ${dbMakefile} stop
                    '';
                }
            )
        );
    };

    pkgs = {
        haskellPackages = pkgs'.haskellPackages.override {
            overrides = haskellOverrides;
        };
    };

in
    pkgs.haskellPackages.databrary
