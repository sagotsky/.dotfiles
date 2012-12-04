#!/bin/sh

DIR="/home/sagotsky/Music"
while [ $? -eq 0 ] && [ -x /usr/bin/inotifywait ] ; do
  inotifywait -r "$DIR" -e OPEN &> /dev/null
  pidof rhythmbox && rhythmbox-client --print-playing
  sleep .2s
done
