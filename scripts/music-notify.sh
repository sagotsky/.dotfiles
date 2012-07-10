#!/bin/sh

while [ $? -eq 0 ] && [ -x /usr/bin/inotifywait ] ; do
  inotifywait -r ~/Music -e OPEN &> /dev/null
  rhythmbox-client --print-playing
  sleep 1
done
