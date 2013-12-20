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

notify-send cmus "$PRE${META[artist]} - ${META[title]}" &

# focus follows song
cmus-remote -C win-sel-cur


# folder.jpg display
FILE=$(echo "$STATE" | grep file | cut -f 2- -d' ')
JPG="${FILE%/*}/folder.jpg"
WIDTH=$(xwininfo -root | grep Width | cut -f 2 -d:)
[ -f "$JPG" ] && feh -x "$JPG" -g 200x200+$(( WIDTH/2 -210 ))+16 -B black 2>/dev/null & # get resolution instead of hard coding
sleep 2 ; killall feh

# last.fm
#[[ `which zomg` ]] && zomg "$(echo "$STATE" | grep '^file' | cut -f 2- -d' ' )"
