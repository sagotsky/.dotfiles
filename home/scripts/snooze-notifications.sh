#!/bin/bash

# Temporarily snooze dunst notifications.  Gmail too?

snooze() {
  killall -s STOP dunst
}

unsnooze() {
  echo trapped
  killall -s CONT dunst
  exit
}

DURATION=$1

trap unsnooze HUP INT QUIT TERM

snooze
  sleep $DURATION &
  wait $!
unsnooze
