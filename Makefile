#
# COMMON TASKS
#

## The default action is to run tests
cabal.test: ; nix-shell -Q --cores 4 --run 'cabal -j new-test'
.PHONY: cabal.test

## Start the db (needed for cabal.test and cabal.build, but that relationship is
## not captured by Make yet)
db: ; nix-shell -Q --run ./init-db-pql.sh
.PHONY: db

## Start the dev repl
devel: node ; nix-shell --run ghci-databrary
.PHONY: devel

## One can always build with Nix.
nix.build: ; nix-build -A databrary -Q
.PHONY: nix.build

## You can also build with Cabal if that suits you
cabal.build: ; nix-shell -Q --cores 4 --run 'cabal -j new-build'
.PHONY: cabal.build

#
# Experimental tasks
#

##
## These may eventually be used to auto-generate databrary.cabal
##

module_list.yaml:
	echo -e '_modules:\n- Paths_databrary' > $@
	find src -regex '.*\.hsc?' >> $@
	sed -i -e 's@src/\(.*\).hs.*@- \1@' -e 's@/@.@g' $@
.PHONY: module_list.yaml

package.yaml: module_list.yaml

databrary.cabal: package.yaml ; hpack .
replTest: ; nix-shell --run 'cabal configure -freplTest --datadir=. --enable-tests; cabal repl test:databrary-test'
.PHONY: replTest

##
## This is the beginning of packaging up node deps
##

nodeFiles := $(addprefix node/, default.nix node-env.nix node-packages.nix)

node: $(nodeFiles)
.PHONY: node

$(nodeFiles) : node/package.json
	cd node && nix-shell -Qp nodePackages.node2nix --run node2nix
