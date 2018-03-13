#!/bin/bash
cp -R src/ app-databrary/ app-generate/ databrary.cabal runtime-dirs/databrary-inttest/
mkdir -p runtime-dirs/databrary-inttest/config
cp messages.conf volume.json runtime-dirs/databrary-inttest/    # copied from default.nix/databrary.nix
cp -R web/ runtime-dirs/databrary-inttest/
cp transcode transctl.sh runtime-dirs/databrary-inttest/
mkdir -p runtime-dirs/databrary-inttest/solr/
cp -r solr/solr.xml solr/log4j.properties solr/conf runtime-dirs/databrary-inttest/solr
cat runtime-dirs/databrary-inttest/reload-gen-start-inttest | nix-shell --attr databrary-dev.env default.nix --command "cd runtime-dirs/databrary-inttest && ghci-databrary-inttest"

