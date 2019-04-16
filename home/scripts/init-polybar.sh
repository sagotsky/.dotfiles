#!/bin/bash

# Runs polybar, redirecting stdin into named pipe polybar will read from later

killall polybar &>/dev/null
there-can-be-only-one.sh

FIFO='/tmp/.polybar.fifo'
rm $FIFO
mkfifo $FIFO

polybar example &

while read -r line ; do
  echo "$line" > $FIFO
done
