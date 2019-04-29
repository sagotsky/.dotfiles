#!/bin/bash

# Runs polybar, redirecting stdin into named pipe polybar will read from later

killall polybar &>/dev/null
there-can-be-only-one.sh

FIFO='/tmp/.polybar.fifo'
rm $FIFO &>/dev/null
mkfifo $FIFO

export PRIMARY_MONITOR="$(xrandr -q | grep primary | cut -f1 -d' ')" 

if grep $(hostname) ~/.config/polybar/config &> /dev/null ; then
  BAR="$(hostname)"
else
  BAR="example"
fi

polybar $BAR &

while read -r line ; do
  echo "$line" > $FIFO
done
