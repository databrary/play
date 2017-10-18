#!/bin/bash
set -e
build_user="centos"
clone_path="~$build_user/src/databrary"
exe_dir="~$build_user/.cabal/bin"
data_basedir="~$build_user/.cabal/share/x86_64-linux-ghc-7.10.3"
branch="develop"

cd $clone_path
echo "=== Check branch matches required branch in ../databrary-branch.txt"


echo "=== Current working copy status"
git status

echo "=== Stash, pull latest"
git stash save # need in case there were manual, conflicting changes to prevent pulling from succeeding
git pull

echo "=== Run new db migrations, build, install"
./dev
builtexe=`ls -lt $exe_dir/databrary-* | head -1` #specify this to a specific version

echo "=== Start"
ls databrary.conf
databrary_datadir="$data_basedir/databrary-1" `$builtexe`
