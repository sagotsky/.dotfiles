#!/bin/bash

duration='.1s'
color='red'

INFO="$(xwininfo -id `xprop -root | grep '^_NET_ACTIVE_WINDOW(WINDOW)' | cut -f5 -d' '`)"
WIDTH=$(echo "$INFO" | grep Width | cut -f 2 -d:)
HEIGHT=$(echo "$INFO" | grep Height | cut -f 2 -d:)
X=$(echo "$INFO" | grep 'Absolute upper-left X' | cut -f 2 -d:)
Y=$(echo "$INFO" | grep 'Absolute upper-left Y' | cut -f 2 -d:)

echo '' | dzen2  -bg $color -p 1 -y $Y -x $X -w $WIDTH -h $HEIGHT  & PID=$! ; sleep $duration ; kill $PID
