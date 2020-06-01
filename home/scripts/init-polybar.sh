#!/bin/bash

# Runs polybar, redirecting stdin into named pipe polybar will read from later

killall polybar &>/dev/null
there-can-be-only-one.sh

refresh-monitors.sh # does this belong elsewhere?  it needs to hit before this script, but during seems wrong

FIFO='/tmp/.polybar.fifo'
rm $FIFO &>/dev/null
mkfifo $FIFO


if grep "$(hostname)" ~/.config/polybar/config &> /dev/null ; then
  BAR="$(hostname)"
else
  BAR="example"
fi

# WM="$(grep . ~/.xprofile | grep -v '^#' | tail -n 1)"
# WM_BAR="bar/$WM"
# if grep "$WM_BAR" ~/.config/polybar/config &> /dev/null ; then
#   export WM_BAR
# fi
# inherit = ${env:WM_BAR:bar/base} ; mlutiple inheritance isn't actually working.


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
