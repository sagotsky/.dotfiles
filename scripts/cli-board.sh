#!/bin/bash

DIR="$HOME/.cli-board/"
DMENU_OPTS=''
FILE=$(find $DIR -type f -printf '%f\n'  | dmenu $DMENU_OPTS)
export DISPLAY=:0.0

if [ -x "$DIR/$FILE" ] ; then
  echo $($DIR/$FILE) | xsel -i
else 
  cat "$DIR/$FILE" | xsel -i
fi

