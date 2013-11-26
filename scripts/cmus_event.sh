#!/bin/bash

# Prints metadata from song on cmus events
# To enable, in cmus use `:set status_display_program=/path/to/cmus_event.sh`

[[ $2 == 'playing' ]] && PRE='' || PRE='■ '

declare -A META
STATE=$(cmus-remote -Q)
for tag in artist album title date genre tracknumber albumartist ; do
  META[$tag]=$(echo "$STATE" | grep "^tag $tag" | cut -d' ' -f 3- )
done

notify-send [cmus] "$PRE${META[artist]} - ${META[title]}"
