#!/bin/bash

# wrapper for notin
# sends notification updates to dzen2
# may do some parsing while we're here

# apps with an _appname() function will have that performed instead of dzen

function _rhythmbox() {
  rhythmbox-client  --print-playing-format '%aa - %tt' > ~/.music.out &
}

function _nuvolaplayer() {
  music-client.sh bandsong > ~/.music.out &
}

function _cmus() {
  echo $@ | cut -f 2- -d':'> ~/.music.out &
}


# show chat differently
function _pidgin() {
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
[[ "$1" == '-d' ]] && DEBUG=1

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

notin.py | while read line ; do
  if [[ "$line" ]] ; then
    line=$(echo $line | sed -e 's/\[notify-send\] \(.*\):/[\1]:/')
    app=$(echo $line | sed -e 's/\[\(.*\)\].*/\1/' | tr -d ' ' | tr [:upper:] [:lower:]  )
    func="_$app"

    if [[ $(type -t $func) == 'function' ]] ; then
      $func $line & #|| format $line | dzen2 $DZEN_OPTS &
    else 
      format $line | dzen2 $DZEN_OPTS &
    fi

    [[ "$DEBUG" == '1' ]] && echo -e "Func: $func\nLine: $line\n"
  fi
done 
