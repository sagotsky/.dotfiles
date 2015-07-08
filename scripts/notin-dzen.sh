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
  SONG_INFO="$(echo $@ | cut -f 2- -d':')"
  echo ${SONG_INFO:0:80} > ~/.music.out &
}


# show chat differently
function _pidgin() {
# iff pidgin is not active window!
  DZEN_OPTS=" -bg linen -fg black -xs 1 -ta l -fn fixed-8 -p 5 -u "
  echo $@ |
    sed -e "s/^\[\(.*\)\]/^fg(darkgoldenrod)\[\1\]^fg()/" |
    sed -e "s/<i>/^fg(khaki)/g" |
    sed -e 's/<\/i>/^fg()/g' |
    dzen2 $DZEN_OPTS

  dzen-clear.sh
}

#colors
C_PROGRAM='darkgoldenrod'
C_ITALICS='khaki'
TIME=4
DZEN_OPTS=" -bg darkslategray -fg white -xs 1 -ta l -fn fixed-8 -p $TIME -u "
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

# kill other daemons
for daemon in dunst notification-daemon notify-osd notin.py xfce4-notifyd ; do
  killall -9 $daemon &> /dev/null
done

# there can be only one
pidof -x $0 | sed -e "s/$$//" | xargs kill &>/dev/null

notin.py | while read line ; do
  if [[ "$line" ]] ; then
    line=$(echo $line | sed -e 's/\[notify-send\] \(.*\):/[\1]:/')
    app=$(echo $line | sed -e 's/\[\([a-zA-Z0-9 ]*\)\].*/\1/' | tr -d ' ' | tr [:upper:] [:lower:]  )
    func="_$app"

    if [[ $(type -t $func) == 'function' ]] ; then
      $func $line & #|| format $line | dzen2 $DZEN_OPTS &
    else 
      format $line | dzen2 $DZEN_OPTS &
    fi

    [[ "$DEBUG" == '1' ]] && echo -e "Func: $func\nLine: $line\n"
  fi
done 
