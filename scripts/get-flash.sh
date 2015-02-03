#!/bin/sh -e

function file_url() {
  curl -s "http://get.adobe.com/flashplayer/download/?installer=Flash_Player_11.2_for_other_Linux_(.tar.gz)_64-bit&standalone=1" | grep tar.gz | tr '"' "\n" | tr "'" "\n" | grep http
}

LIBDIR="/usr/lib/mozilla/plugins/" # should this be overridable?
DIR=$(mktemp -d)
cd $DIR
wget $(file_url)
tar -xzf *.tar.gz
ls *.so

sed -i 's/_NET_ACTIVE_WINDOW/_ZZZ_ACTIVE_WINDOW/' libflashplayer.so
sudo mv $LIBDIR/libflashplayer.so{,-backup-$(date +%s)}
sudo mv libflashplayer.so $LIBDIR/
# change owner?
