#!/bin/bash

# Temporarily snooze dunst notifications.  Gmail too?

snooze() {
  killall -s STOP dunst
}

unsnooze() {
  killall -s CONT dunst
  exit
}

DURATION=$1

trap unsnooze HUP INT QUIT TERM

if snoozing-notifications.sh ; then
  unsnooze
else
  snooze
fi
