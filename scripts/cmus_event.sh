#!/bin/bash

# Prints metadata from song on cmus events
# To enable, in cmus use `:set status_display_program=/path/to/cmus_event.sh`

[[ $2 == 'playing' ]] && PRE='' || PRE='■ '

# notification
declare -A META
STATE=$(cmus-remote -Q)
for tag in artist album title date genre tracknumber albumartist ; do
  META[$tag]=$(echo "$STATE" | grep "^tag $tag" | cut -d' ' -f 3- )
done
META['position']=$(echo "$STATE" | grep ^position | cut -d ' ' -f 2)
FILE=$(echo "$STATE" | grep file | cut -f 2- -d' ')

notify-send cmus "$PRE${META[artist]} - ${META[title]}" &

# focus follows song
cmus-remote -C win-sel-cur


# folder.jpg display
if [[ "$2" == 'playing' ]] ; then
  JPG="${FILE%/*}/folder.jpg"
  WIDTH=$(xwininfo -root | grep Width | cut -f 2 -d:)
  [ -f "$JPG" ] && feh -x "$JPG" -g 200x200+$(( WIDTH/2 -210 ))+16 -B black 2>/dev/null & # get resolution instead of hard coding
  ( sleep 2 ; killall feh ) &

  # back it rxvt's background
  echo $JPG
  [[ -p '/tmp/background-urxvt' ]] && echo "$JPG" >> /tmp/background-urxvt && echo $JPG
fi

# last.fm
[[ $(which scrobbler.pl) && "${META['position']}" -lt 1 ]] && echo "$FILE" | scrobbler.pl $(cat $HOME/.scrobblerrc) 

true
