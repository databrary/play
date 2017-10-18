#!/bin/bash
set -e
build_user="centos"
clone_path="/home/$build_user/src/databrary"
exe_dir="/home/$build_user/.cabal/bin"
data_basedir="/home/$build_user/.cabal/share/x86_64-linux-ghc-7.10.3"

cd $clone_path
branch=`cat ../databrary-branch.txt`
echo "=== Check branch matches required branch in ../databrary-branch.txt"
current_branch=`git rev-parse --abbrev-ref HEAD`
echo "Current branch is $current_branch."

echo "=== Current working copy status"
git status

echo "=== Stash, pull latest"
git stash save # need in case there were manual, conflicting changes to prevent pulling from succeeding
git pull

echo "=== Run new db migrations, build, install"
./dev
builtexe=`ls -t $exe_dir/databrary-* | head -1` #extract exact version from git describe instead

echo "=== Start"
ls databrary.conf
databrary_datadir="$data_basedir/databrary-1" `$builtexe`
