#!/bin/bash

# wrapper for notin
# sends notification updates to dzen2
# may do some parsing while we're here

#colors
C_PROGRAM='darkgoldenrod'
C_ITALICS='khaki'

TIME=3
DZEN_OPTS=" -bg darkslategray -fg white -xs 1 -ta l -fn 6x12 -p $TIME -u "

function height() {
  xwininfo -root |\
    grep Height |\
    cut -f4 -d' '
}

export HEIGHT=$(height)
#DZEN_OPTS="$DZEN_OPTS -y ` expr $HEIGHT - 15 `"

function format() {
  echo $@ |\
    sed -e "s/^\[\(.*\)\]/^fg($C_PROGRAM)\[\1\]^fg()/" |\
    sed -e "s/<i>/^fg($C_ITALICS)/g" |\
    sed -e 's/<\/i>/^fg()/g' 
}

killall notification-daemon &> /dev/null

notin.py | while read line ; do
  if [[ "$line" ]] ; then
    format $line | dzen2 $DZEN_OPTS 
  fi
done 
