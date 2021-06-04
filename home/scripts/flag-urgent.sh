#!/bin/sh

# wmctrl -r "$@" -b remove,demands_attention
# sleep 0.1
# wmctrl -r "$@" -b add,demands_attention

app=$1
xwininfo -root -tree | grep $app | awk '{print $1}' | while read id ; do
  xdotool set_window --urgency 0 $id
  sleep 0.05
  xdotool set_window --urgency 1 $id
done
