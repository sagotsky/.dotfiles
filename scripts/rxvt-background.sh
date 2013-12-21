#!/bin/bash

# Watch a an image file.  When it changes, set the background image to that.

# there can be only one
pidof -x $0 | sed -e "s/$$//" | xargs kill &>/dev/null 

FIFO=/tmp/background-urxvt
rm $FIFO 2>/dev/null
mkfifo $FIFO
   

while : ; do
  if read line <$FIFO ; then
    [[ -f "$line" ]] && convert "$line" -fill black -colorize 80% /tmp/rxvt.jpg ; printf "\E]20;%s;100\a" /tmp/rxvt.jpg
  fi
  sleep 1
done 


