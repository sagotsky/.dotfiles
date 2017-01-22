#!/bin/sh

app=$1
xwininfo -root -tree | grep $app | awk '{print $1}' | while read id ; do
  xdotool set_window --urgency 0 $id
  sleep 0.05
  xdotool set_window --urgency 1 $id
done
