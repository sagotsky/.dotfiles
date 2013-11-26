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

function _cmus() {
# app is fetched wrong...
  echo $@ | cut -f 2- -d':'> ~/.music.out &
}


# show chat differently
function _Pidgin() {
  DZEN_OPTS=" -bg linen -fg black -xs 1 -ta l -fn termsyn-8 -p 5 -u "
  echo $@ |
    sed -e "s/^\[\(.*\)\]/^fg(darkgoldenrod)\[\1\]^fg()/" |
    sed -e "s/<i>/^fg(khaki)/g" |
    sed -e 's/<\/i>/^fg()/g' |
    dzen2 $DZEN_OPTS &
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

for daemon in notification-daemon notify-osd notin.py  ; do
  killall -9 $daemon &> /dev/null
done


#notin.py | while read line ; do
notin.py | while read line ; do
  if [[ "$line" ]] ; then
    app=$(echo $line | tr -d '[]' | cut -f1 -d' ')

    # replace notify-send with another app if present
    [[ $app == 'notify-send' ]] && app2=$(echo $line | tr -d '[]' | cut -f1 -d':' | cut -f2 -d' ')
    [[ $app2 != '' ]] && app="$app2" && line=$(echo $line | sed -e 's/\[notify-send\] //')

    func=$(echo "_$app" | tr -d ' ')
    if [[ $(type -t $func) == 'function' ]] ; then
      $func $line || format $line | dzen2 $DZEN_OPTS &
    else 
      format $line | dzen2 $DZEN_OPTS &
    fi
  fi
done 
