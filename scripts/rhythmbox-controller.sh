#!/bin/sh

# lets other users control my rhythmbox
# reads commands in from a fifo.  other users should write to that fifo.
# commands listed in case are passed to rhythmbox-client as arguments
# this should be run at the start of an x session or any time you want to share sound

FIFO="/tmp/rhythmbox-$USER"
rm $FIFO
mkfifo $FIFO
chgrp audio $FIFO
chmod g+w $FIFO

while [ true ] ; do
    CMD=$(cat $FIFO)
    case $CMD in
        --play-pause|--next|--previous)
        rhythmbox-client $CMD
        ;;
    esac
done


