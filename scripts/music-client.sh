#!/bin/bash

# front end to whatever music player is currently playing

APPS=('rhythmbox' 'nuvolaplayer' 'cmus')
CMDS=('volup' 'voldown' 'mute' 'play' 'back' 'toggle' 'status' 'bandsong') # rate1-5 (thumbs up or down depending on value?)

# add volup commands.  make them print status.  empty status if no playing.

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

cmus() {
  case $1 in
    play) cmus-remote -p   ;;
    next) cmus-remote -n   ;;
    back) cmus-remote -b   ;;
    toggle | pause) cmus-remote -u ;;
    status) cmus-remote -Q ;;
    *) return 1
  esac
}

rhythmbox() {
  case $1 in
    play | pause | next) rhythmbox-client --$1 ;;
    back) rhythmbox-client --previous ;;
    toggle) rhythmbox-client --play-pause ;;
    status) rhythmbox-client --print-playing ;;
    bandsong) rhythmbox-client  --print-playing-format '%aa - %tt' ;;
    rate1) rhythmbox-client --set-rating 1 ;;
    rate2) rhythmbox-client --set-rating 2 ;;
    rate3) rhythmbox-client --set-rating 3 ;;
    rate4) rhythmbox-client --set-rating 4 ;;
    rate5) rhythmbox-client --set-rating 5 ;;
    *) return 1
  esac
}

nuvolaplayer() {
  case $1 in
    play | pause | next) nuvolaplayer-client $1 ;;
    back) nuvolaplayer-client prev ;;
    toggle) nuvolaplayer-client toggle ;;
    status) nuvolaplayer-client status ;;
    bandsong) echo $(nuvolaplayer-client status | grep '^Artist' | cut -f2 -d:) - $(nuvolaplayer-client status | grep '^Song' | cut -f2 -d:) ;;
    *) return 1
  esac
}

player=$(getplaying) && for cmd in "$@" ; do
  $player $cmd || global_commands $cmd
done
# False $1 doesn't actually work
