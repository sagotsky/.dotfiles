#!/bin/sh

# selects a folder, sends it to cmus

SELECT="dmenu -i -l 20 -b"
if [[ "$1" == '-r' ]] ; then
  SELECT="shuf -n 1"
fi


DIR=$(find $HOME/Music/ -type d | sort | $SELECT )
DIR=${DIR%/}    # strip final /
DIR=${DIR##*/}  # strip up to first /

if [[ "$DIR" != "" ]] ; then
  cmus-remote -C "live-filter ~f */$DIR/*"
  cmus-remote -C "echo Playing: $DIR"
  cmus-remote -n
fi
