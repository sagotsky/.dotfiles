#!/bin/sh

while [ $? -eq 0 ] && [ -x /usr/bin/inotifywait ] ; do
  inotifywait -r ~/Music -e OPEN &> /dev/null
  sleep 1
  rhythmbox-client --print-playing
done
