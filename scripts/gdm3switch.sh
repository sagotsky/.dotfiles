#!/bin/bash

# switch to a user or make a new login screen

if [ $# != 1 ] ; then
  echo 'Specify a user'
  exit 1
fi

function get_display() {
  w | grep "^$1" | awk '{print $2}' | grep ':'
}

function get_tty() {
  [[ $# == 1 ]] && (
    ps ax -o tty,cmd | grep "[X] $1" | awk '{print $1}' | tr -d tty
  )
}


DISPLAY="$(get_display $1)"
TTY="$(get_tty $DISPLAY)"
echo "display: $DISPLAY tty: $TTY"

if [ "$TTY" == "" ] ; then
  pidof gdm3    && gdmflexiserver 
  pidof lightdm && echo dm-tool switch-to-user $1
else
  sudo chvt $TTY
fi
