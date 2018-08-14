(NixOS or Ubuntu)

Some steps to use to query around and understand whether the nix cache
is being used as expected or not..



**The bellwether**: is GHC being built? Then your cache has failed you.



## Which cache?



Don't use obsidian.webhop.com. Use nixcache.reflex-frp.org.

  - Obsidian had set up one for Databrary at
    obsidian.webhop.com:\#\#\#\#; however, it is stale and doesn't
    support our newer nixpkgs
  - Since we still use reflex-platform, we still rely on
    Reflex/Obsidian's cache
  - Maybe some day we'll get rid of the dependency on reflex-platform
    (we don't use reflex) and go back to using the standard NixOS cache
    as our upstream
  - But we should have our own cache, fed by CI!

## Some commands that proved useful



    nix-store -q --tree {...}

Lists all dependencies for a particular thing in the nix store



    nix-instantiate -A databrary-dev.env

Shows the path to the Nix derivation (build command) generated for the
dev environment

EDIT: `nix-instantiate` is actually not very useful for
troubleshooting caches. It actually triggers a build of most
dependencies, which is precisely what we're trying to avoid.



    nix-store -q --tree $(nix-instantiate -A databrary-dev.env) | tee deps

Combines the previous two commands to list all deps for the dev
environment



```bash
for hsh in `nix-store -qR $(nix-instantiate -A databrary-dev.env)|perl -pe 's%/nix/store/(\w{32,32}).*%$1%'`
do
    curl '-#' -fI http://obsidian.webhop.org:8950/${hsh}.narinfo >/dev//null
    echo -n "$? - $hsh"
    sleep 0.10
done | tee deps-available
```

List all deps, then send a HEAD request to the cache to see if each dep
is available.

## Open Questions


When I expect all my deps to have binary substitutions available in the
cache, how can my expectation be broken?

1.  Same nix expressions?
2.  Same nixpkgs?
3.  Cache online?
4.  Cache populated?
5.  Same architecture?



How do I binary search on a tree of derivations to find the first one
that diverges? (Or can I jump straight to it?)
