#!/bin/bash
cabal configure --enable-tests
cabal install --only-dependencies --enable-tests
cabal test
