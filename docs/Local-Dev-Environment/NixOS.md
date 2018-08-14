If you are on NixOS, don't modify /etc/nix/nix.conf directly. Instead, add the
following to /etc/nixos/configuration.nix:

    nix.binaryCaches = [
      "http://devdatabrary2.home.nyu.edu:5000/"
    ];
    nix.binaryCachePublicKeys = [
      "devdatabrary2.home.nyu.edu-1:xpI1XOvf7czNv0+0/1ajpgotpOnUMTUBBF9v97D5/yk="
    ];

and then rebuild your configuration with nixos-rebuild. (If you don't know what
I'm talking about, read the man pages, or consider sacrificing chickens to the
appropriate gods).
