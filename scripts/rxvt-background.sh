#!/bin/bash

# Watch a an image file.  When it changes, set the background image to that.

# there can be only one
pidof -x $0 | sed -e "s/$$//" | xargs kill &>/dev/null 

FIFO=/tmp/background-urxvt
rm $FIFO 2>/dev/null
mkfifo $FIFO
ALBUMART="$HOME/.cmus/albumart"
mkdir "$ALBUMART" 2> /dev/null
   

while : ; do
  if read LINE <$FIFO ; then
    if [[ -f "$LINE" ]] ; then
      CACHED="$ALBUMART/$(echo ${LINE#$HOME/} | tr '/' '_')"
      [[ -f "$CACHED" ]] || convert "$LINE" -fill black -colorize 80% "$CACHED"
      printf "\E]20;%s;100\a" "$CACHED"
    fi
  fi
  sleep 1
done 


