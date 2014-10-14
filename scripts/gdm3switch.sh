#!/bin/bash

# switch to a user or make a new login screen

if [ $# != 1 ] ; then
  echo 'Specify a user'
  exit 1
fi

function get_display() {
  USER="$1"
  w | grep "^$USER.*gdm-session-worker" | cut -f 6 -d' '
}

function get_tty() {
  [[ $# == 1 ]] && (
    DISPLAY="$1"
    ps ax -o tty,cmd | grep "[X]org $DISPLAY" | cut -f 1 -d' ' | tr -d tty
  )
}


DISPLAY="$(get_display $1)"
TTY="$(get_tty $DISPLAY)"

if [ "$TTY" == "" ] ; then
  gdmflexiserver 
else
  echo $TTY
  sudo chvt $TTY
fi
