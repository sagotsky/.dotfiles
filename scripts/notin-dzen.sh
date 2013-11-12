#!/bin/bash

# wrapper for notin
# sends notification updates to dzen2
# may do some parsing while we're here

# apps with an _appname() function will have that performed instead of dzen

function _Rhythmbox() {
  rhythmbox-client  --print-playing-format '%aa - %tt' > ~/.music.out &
}

#function _NuvolaPlayer() {
#  echo "$@"
#  return 1
#}

function _Nuvola() {
# app is fetched wrong...
  music-client.sh bandsong > ~/.music.out &
}



#colors
C_PROGRAM='darkgoldenrod'
C_ITALICS='khaki'

TIME=5
DZEN_OPTS=" -bg darkslategray -fg white -xs 1 -ta l -fn termsyn-8 -p $TIME -u "

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

for daemon in `echo notification-daemon notify-osd notin.py` ; do
  killall daemon &> /dev/null
done


#notin.py | while read line ; do
notin.py | while read line ; do
  if [[ "$line" ]] ; then
    app=$(echo $line | tr -d '[]' | cut -f1 -d' ')
    func=$(echo "_$app" | tr -d ' ')
    if [[ $(type -t $func) == 'function' ]] ; then
      $func $line || format $line | dzen2 $DZEN_OPTS &
    else 
      format $line | dzen2 $DZEN_OPTS &
    fi
  fi
done 
