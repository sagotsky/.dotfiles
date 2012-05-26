#!/bin/bash

# move shows from ddwrt showrss to local storage
# maybe someday this can be done at bootup

DDWRT='freewifi'                  # server
SHOWS='/mnt/rtorrent/done/'       # path on server
DEST='/media/raid/video/rsstv/'   # local destination
TMP=$(mktemp -d)

# Don't run if another process is running
COUNT=$(pidof -x showrss_move.sh )
C=$(echo $COUNT | wc -w)
if [[ $C -gt 1 ]] ; then
	exit
fi

# copy each file, then delete and move.
for FILE in $(ssh $DDWRT -q ls $SHOWS 2>/dev/null | grep -v xauth) ; do
  if scp -r "$DDWRT:$SHOWS/$FILE" "$TMP" ; then
    ssh $DDWRT rm "$SHOWS/$FILE"
    chown sagotsky:sagotsky "$TMP/$FILE"
    mv "$TMP/$FILE" "$DEST"
  fi
done

# and make sure rtorrent is still running.
ssh freewifi pidof rtorrent || service rtorrent start

# one file at a time so they can each be copied instead of waiting for all to dl
# give root passwordless access and run at startup

#if scp -r "$DDWRT:$SHOWS/*" "$TMP/" ; then
  #for file in $(ls $TMP) ; do
    #ssh $DDWRT rm "$SHOWS/$file" 
    #chown sagotsky:sagotsky "$TMP/$file"
  #done
  #mv $TMP/* $DEST/
#fi
