# Steps for updating node deps

Perform all steps in this node/ subdirectory.

1. Use npm to get the versions you want and modify package.json
2. Run node2nix, using a version of node2nix >= 1.5.1
   * Note: Version 1.2 generated supplement.nix, which is now superfluous
     and has been removed.
3. Check in all changes to this directory
