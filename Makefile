# Editing this Makefile? run make with `BUILDDEV=1 make` for better build
# debugging.

# Not everything is cached in our own cache, so use both
NIX_OPTIONS := --option binary-caches "https://cache.nixos.org http://devdatabrary2.home.nyu.edu:5000"

# These below intentionally use '='to pick up following changes to NIX_OPTIONS
nix-build-args = $(NIX_OPTIONS) --drv-link $(PWD)/derivation --cores 0 -A databrary
nix-shell-args = $(NIX_OPTIONS) #--pure Commented for now because git DNE

ifdef BUILDDEV
nix-build-args += --keep-failed
endif

# Sneaky options used in recursing make. Not for human consumption.
ifdef __COVERAGE
nix-build-args += --arg coverage true
endif
ifdef __HADDOCK
nix-build-args += --arg haddock true
endif

nix-shell = nix-shell $(nix-shell-args)
nix-build = nix-build $(nix-build-args)

#
# COMMON TASKS
#

## The default action is to run tests
cabal-test: ; $(nix-shell) --run 'cabal -j test --ghc-options="-O0"  --test-options="--color always --hide-successes --timeout 1s"'
.PHONY: cabal-test

## Start the db (needed for cabal.test and cabal.build, but that relationship is
## not captured by Make yet)
db: ; $(nix-shell) --run ./init-db-pql.sh
.PHONY: db

## Start the dev repl
repl: ; $(nix-shell) --run ./ghci-databrary.sh
.PHONY: repl

## Start tests in the repl
repl-test: ; $(nix-shell) --run 'cabal repl test:discovered'
.PHONY: repl-test

## Run configure for repltastic tasks
repl-config: ; $(nix-shell) --run 'cabal configure --datadir=. --datasubdir=. --disable-optimization --enable-tests'
## One can always build with Nix.
nix-build: ; $(nix-build)
.PHONY: nix-build

## You can also build with Cabal if that suits you
cabal-build: ; $(nix-shell) --run 'cabal -j new-build --disable-optimization'
.PHONY: cabal-build

#
# Experimental tasks
#

# Dump splices and list the newly creates dump files.
splices:
	$(nix-shell) --run 'cabal -j new-build lib:databrary --ghc-options "-ddump-splices -ddump-to-file"'
	@find dist-newstyle -name '*.dump-splices' -mmin -10
.PHONY: splices

##
## For haddock development
##
haddock:
	$(nix-shell) --run 'cabal haddock --hyperlink-source'
.PHONY: haddock

##
## These may eventually be used to auto-generate databrary.cabal
##

module_list.yaml:
	echo -e '_modules:\n- Paths_databrary' > $@
	find src -regex '.*\.hsc?' >> $@
	sed -i -e 's@src/\(.*\).hs.*@- \1@' -e 's@/@.@g' $@


testFiles := $(shell find test -name '*Test.hs')
tests_module_list.yaml: $(testFiles)
	echo -e '_modules:\n' > $@
	find test -regex '.*\.hsc?' >> $@
	sed -i -e 's@test/\(.*\).hs.*@- \1@' -e 's@/@.@g' $@

tests_module_list.cabalsnip: tests_module_list.yaml
	yq -r '.[]|join("\n")' < $< > $@

package.yaml: module_list.yaml

databrary.cabal: package.yaml ; hpack .

##
## This is the beginning of packaging up node deps
##

nodeFiles := $(addprefix node/, default.nix node-env.nix node-packages.nix)

node: $(nodeFiles)
.PHONY: node

$(nodeFiles) : node/package.json
	cd node && nix-shell -Qp nodePackages.node2nix --run node2nix
