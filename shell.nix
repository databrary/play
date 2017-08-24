let
inherit (import ./build.nix {}) pkgs nixpkgs nodePackages;

#TODO import node commands to nix-shell environment
in

pkgs.databrary-dev.env.overrideAttrs (attrs: {
  buildInputs = attrs.buildInputs or [] ++
    [nixpkgs.ffmpeg
     nodePackages.shell.nodeDependencies
     nixpkgs.nodejs-8_x
     nixpkgs.postgresql96
     nixpkgs.jdk #required by Solr
     nixpkgs.haskellPackages.cabal-install
    ];
})
