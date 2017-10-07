# this is just for fdkaac
#!/bin/bash
set -e
CURRENTUSER=$USER
dockerComposeVersion=1.14.0
# need root for installs
sudo -s <<EOF

# for fdk-aac
sed -i "/^# deb.*multiverse/ s/^# //" /etc/apt/sources.list
echo 'deb http://us.archive.ubuntu.com/ubuntu/ xenial multiverse' >> /etc/apt/sources.list
echo 'deb http://us.archive.ubuntu.com/ubuntu/ xenial-updates multiverse' >> /etc/apt/sources.list

# deps
apt-get update && apt-get install -y libgmp-dev git yasm npm libcrack2-dev gcc g++ autoconf automake zlib1g-dev \
		   libmp3lame-dev libx264-dev libfdk-aac-dev libavformat-dev libswscale-dev libavcodec-dev libavutil-dev curl \
		   pkg-config vim ssmtp postgresql-client libgmp3-dev libz-dev git vim
		   

curl -L https://github.com/docker/compose/releases/download/$dockerComposeVersion/docker-compose-`uname -s`-`uname -m` > /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose

# nonstandard node binary
ln -s /usr/bin/nodejs /usr/bin/node

# compile ffmpeg from source
cd /usr/src
git clone git://source.ffmpeg.org/ffmpeg.git
cd ffmpeg
git remote update origin
ffmpeg=2.8.x && ffmpeg=`git describe --abbrev=0 origin/release/${ffmpeg%.x}` && ffmpeg=${ffmpeg#u} && git checkout $ffmpeg
./configure --enable-gpl --enable-version3 --enable-nonfree --enable-libx264 --enable-libfdk-aac --enable-libmp3lame # --extra-ldexeflags='-lcr_-u cr_run_link_me'
make && make install

#get docker
wget -qO- https://get.docker.com/ | sh
usermod -aG docker $CURRENTUSER
su - $CURRENTUSER
su - $CURRENTUSER

