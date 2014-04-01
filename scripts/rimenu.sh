#!/bin/bash

# Select cached ri output from dmenu for fast doc lookup

eval "$(argh.sh $@)" 

CACHE="${CACHE:-}"                 #- Regenerate cache
CACHE_FILE="$HOME/.rimenu"
[ -f "$CACHE_FILE" ] || CACHE='true'

if [ "$CACHE" == "" ] ; then
  TERM=$(dmenu -l 20 -i -b < "$CACHE_FILE" )
  [[ "$TERM" != "" ]] && xterm -bg '#202025' -e ri $TERM
else
  echo 'Building cache...'
  rm $CACHE_FILE
  ri -l | xargs -P 8 -n1 -I% ri %# >> $CACHE_FILE
fi
