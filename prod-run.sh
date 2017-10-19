#!/bin/bash
set -e
build_user="build"
run_user="databrary"
run_working_dir="/home/$run_user"
exe_dir="/home/$build_user/.cabal/bin"
data_basedir="/home/$build_user/.cabal/share/x86_64-linux-ghc-7.10.3"

echo "=== Changing to working directory $run_working_dir"
cd $run_working_dir

echo "=== Determine installed exe to run"
built_exe=`ls -t $exe_dir/databrary-* | head -1` #extract exact version from git describe instead
echo "Newest installed exe found is: $built_exe"

echo "=== Starting $built_exe"
conf_exists=`ls databrary.conf`
databrary_datadir="$data_basedir/databrary-1" `$built_exe`
