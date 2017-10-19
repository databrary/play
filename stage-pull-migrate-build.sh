#!/bin/bash
set -e
build_user="build"
clone_path="/home/$build_user/src/databrary"
exe_dir="/home/$build_user/.cabal/bin"

echo "=== Changing to repository root directory $clone_path"
cd $clone_path

echo "=== Check branch matches required branch"
branch="master"
current_branch=`git rev-parse --abbrev-ref HEAD`
echo "Current branch is $current_branch."
if [ "$branch" != "$current_branch" ]
then
  echo "Expected branch to be $branch. Please correct."
  exit 1
fi

echo "=== Current working copy status"
git status

echo "=== Stash, pull latest from $branch"
git stash save # need in case there were manual, conflicting changes that would prevent pulling from succeeding
git pull

echo "=== Run new db migrations, build, install"
./dev
built_exe=`ls -t $exe_dir/databrary-* | head -1` #extract exact version from git describe instead
echo "Newest installed exe found is: $built_exe"

