#!/usr/bin/env bash
set -e

# this is replacing default.nix installation of this script into nix store, remove that soon
# default.nix should still reference the nix store items this depends on!!
git_root_dir=$(git rev-parse --show-toplevel)
if [ ! -d "solr-6.6.0" ]; then
  if [ ! -d "/tmp/solr-6.6.0" ]; then
    pushd /tmp > /dev/null
    wget -qO- http://archive.apache.org/dist/lucene/solr/6.6.0/solr-6.6.0.tgz | tar -zxv
    popd > /dev/null
  fi
  cp -R /tmp/solr-6.6.0 .
fi
if [ ! -d "cracklib" ]; then
  echo download and create cracklib dict
  # wget http://mirror.centos.org/centos/7/os/x86_64/Packages/cracklib-dicts-2.9.0-11.el7.x86_64.rpm
  # {rpm}/bin/rpm2cpio cracklib-dicts-2.9.0-11.el7.x86_64.rpm > tmp/cracklib-dicts-2.9.0-11.el7.x86_64.cpio
  cp ${git_root_dir}/install/cracklib-dicts-2.9.0-11.el7.x86_64.cpio /tmp
  cd /tmp
  cpio -idmv < cracklib-dicts-2.9.0-11.el7.x86_64.cpio
  cd -
  mkdir -p cracklib
  cp -r /tmp/usr/share/cracklib/pw_dict* cracklib
fi
if [ ! -d "node_modules" ]; then
  echo linking node_modules
  ln -sf $(dirname $(which coffee))/../../../lib/node_modules node_modules # FIXME: Bryan
fi
# make store related dirs
mkdir -p cache/tmp stage tmp trans upload
if [ ! -d "store" ]; then
  cp -R ${git_root_dir}/install/store-seed store
fi
if [ ! -d "databrary_logs" ]; then
  mkdir databrary_logs
  touch databrary_logs/solr_log
fi
rm -f config/email # temporary to cleanup cached old file
if [ ! -e "config/email" ]; then
  mkdir -p config
  cp ${git_root_dir}/install/config.email config/email
fi
# rm -rf dist   # add this back when changing ffmpeg versions, c artifacts don't regenerate properly
cabal configure --datadir=. --datasubdir=.
cabal repl lib:databrary
