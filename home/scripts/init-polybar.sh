#!/bin/bash

# Runs polybar, redirecting stdin into named pipe polybar will read from later

killall polybar &>/dev/null
there-can-be-only-one.sh

FIFO='/tmp/.polybar.fifo'
rm $FIFO &>/dev/null
mkfifo $FIFO


if grep "$(hostname)" ~/.config/polybar/config &> /dev/null ; then
  BAR="$(hostname)"
else
  BAR="example"
fi

PRIMARY_MONITOR="$(xrandr -q | grep primary | cut -f1 -d' ')"
export PRIMARY_MONITOR

# scale up for the 4k laptop
if [[ "$BAR" == "bender" ]] ; then
  if [[ "$PRIMARY_MONITOR" == "DP-4" ]] ; then
    export BAR="$BAR-4k"
  else
    export BAR="$BAR-1k"
  fi
fi

polybar $BAR &

while read -r line ; do
  echo "$line" > $FIFO
done
