#!/bin/bash

# Runs polybar, redirecting stdin into named pipe polybar will read from later

killall polybar
there-can-be-only-one.sh

FIFO='/tmp/.polybar.fifo'
rm $FIFO
mkfifo $FIFO

polybar example &

while read -r line ; do
  echo "$line" > $FIFO
done
