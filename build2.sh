cd ~

mkdir src
cd src

# ghc
wget https://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3b-x86_64-deb8-linux.tar.xz && tar -xvf ghc-7.10.3b-x86_64-deb8-linux.tar.xz
cd ghc-7.10.3 && ./configure && sudo make install

cd ~/src

# cabal
wget https://www.haskell.org/cabal/release/cabal-install-1.24.0.2/cabal-install-1.24.0.2.tar.gz && tar -xvf cabal-install-1.24.0.2.tar.gz
cd cabal-install-1.24.0.2 && EXTRA_CONFIGURE_OPTS= sudo ./bootstrap.sh --global
export PATH=$PATH:/usr/local/bin
cabal update

cd ~/src

# get databrary
git clone git://github.com/databrary/databrary

# make postgres and solr containers
cd ~/src/databrary/Docker
docker volume create --name databrary_postgres_store
docker build -t databrary_postgres postgres/
docker run -d -v databrary_postgres_store:/var/lib/postgresql/data -p 5432:5432 --rm --name databrary_postgres databrary_postgres
./wait-for-postgres.sh localhost "docker exec databrary_postgres /usr/local/src/databrary/init-user-db.sh"

docker volume create --name databrary_solr_store
docker build -t databrary_solr solr/
docker run -d -v databrary_solr_store:/opt/solr -p 8983:8983 --rm --name databrary_solr databrary_solr
./wait-for-solr.sh localhost "docker exec databrary_solr solr create -c databrary_core -d /databrary_conf"

cd ~/src/databrary

# cabal sandbox
cabal sandbox init
cabal install happy --force-reinstalls --ghc-options=-O0
cabal install --only-dependencies --force-reinstalls --ghc-options=-O0
cabal configure 
cp example.conf databrary.conf
yes | cabal install --ghc-options=-O0
