#!/bin/bash

# front end to whatever music player is currently playing

APPS=('rhythmbox' 'nuvolaplayer')

getplaying() {
  for app in "${APPS[@]}" ; do
    pidof $app >/dev/null && echo $app && return 0
  done
  return 1
}

global_commands() {
  case $1 in
    volup) ~/scripts/vol-up.sh   ;;
    voldown)~/scripts/vol-down.sh;;
    mute) ~/scripts/vol-mute.sh;;
  esac
}

rhythmbox() {
  case $1 in
    play | pause | next) rhythmbox-client --$1 ;;
    back) rhythmbox-client --previous ;;
    toggle) rhythmbox-client --play-pause ;;
    status) rhythmbox-client --print-playing ;;
    *) return 1
  esac
}

nuvolaplayer() {
  case $1 in
    play | pause | next) nuvolaplayer-client $1 ;;
    back) nuvolaplayer-client prev ;;
    toggle) nuvolaplayer-client toggle ;;
    status) nuvolaplayer-client status ;;
    *) return 1
  esac
}

`getplaying` $1 || global_commands $1

