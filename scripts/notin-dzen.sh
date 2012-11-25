#!/bin/bash

# wrapper for notin
# sends notification updates to dzen2
# may do some parsing while we're here

# apps with an _appname() function will have that performed instead of dzen

function _rhythmbox() {
  rhythmbox-client  --print-playing-format '%aa - %tt' > ~/.music.out &
}

function _nuvolaplayer() {
  return 1
}



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
killall notify-osd  &> /dev/null

#notin.py | while read line ; do
notin.py | while read line ; do
  if [[ "$line" ]] ; then
    app=$(echo $line | tr -d '[]' | cut -f1 -d' ')
    func="_$app"
    if [[ $(type -t $func) == 'function' ]] ; then
      $func $line || format $line | dzen2 $DZEN_OPTS
    else 
      format $line | dzen2 $DZEN_OPTS 
    fi
  fi
done 
