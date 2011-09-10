#!/bin/bash

# Jon Sagotsky 3/10/08
# run this as a cronjob to handle torrents pulled from RSS

#             ratio    hours  port   up    down
BT="en-ctorrent -E 1:1.5 -e 6 -p 34569 -U 20 -D 100 "
#BT="ctorrent -e 0 -p 34569 -U 20 -D 100 "
RSSTV="/home/sagotsky/Videos/rsstv"
LOGFILE="/tmp/autoTorrent.log"

function download() {
    mv -v "$1" "$RSSTV/.incoming/"
    TORRENT=$(echo $1 | sed -e 's/^.*\/\(.*\.torrent\)/\1/')
    cd "$RSSTV/.incoming/"

    echo "Downloading torrent: $TORRENT" >>$LOGFILE

    $BT "$RSSTV/.incoming/$TORRENT"
    if [ $? -ne 1 ] ; then
	echo "ctorrent fail" >> $LOGFILE
    fi


    VID=$(ctorrent -x "$TORRENT" | egrep -i "avi|mpg|mpeg|mp4" | sed -e 's/^<[0-9]*>\W*//' -e 's/\W*\[[0-9]*\]$//')

    A=$(ctorrent -x "$TORRENT")
    B=$(echo $A | egrep -i "avi|mpg|mpeg|mp4")
    C=$(echo $B | sed -e 's/^<[0-9]*>\W*//' -e 's/\W*\[[0-9]*\]$//')

    # BAD JON!  use -d to hceck if $VID is a dir and -f to see if its a file
    # -e exists
    # -f regular file (not dir or dev)
    # -d dir

    if [ -f $RSSTV/.incoming/$VID ] ; then
	echo "$RSSTV/.incoming/$VID is a file  -- $A -- $B -- $C -- $TORRENT -- " >> $LOGFILE
    else 
	VID=$(ctorrent -x "$TORRENT" | grep "Directory" | sed -e 's/Directory: //')
	echo "\n\n$VID \n..is a directory  -- $A -- $B -- $C --" >> $LOGFILE
    fi

    #mv  "$RSSTV/.incoming/$VID" "$RSSTV/"
    rm  "$TORRENT"
    
    echo "Finished $VID" >> $LOGFILE
    echo "" >> $LOGFILE
}

FILE=$(find $RSSTV/.torrents/ -iname "*.torrent" | head -n 1)
if [ "$FILE" != "" ] ; then
    download "$FILE"
fi
