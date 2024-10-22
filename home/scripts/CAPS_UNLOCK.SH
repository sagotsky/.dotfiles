#!/bin/bash -e

if xset q | grep 'Caps Lock:\s\+on' &>/dev/null ; then
  xdotool key Caps_Lock
fi
