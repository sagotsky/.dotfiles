#!/usr/bin/env sh

SELECTED_WINDOW_FG='#aaaaaa'
ACTIVE_WINDOW_FG='#777777'
EMPTY_WINDOW_FG='#555555'
URGENT_WINDOW_FG='#ff5500'

ps ax |
  grep "$(basename $0)" |
  grep -v grep |
  grep -v "$PID" |
  awk '{print $1}' |
  xargs kill &>/dev/null

function hc() {
  herbstclient $@
}


color() {
  polybar_fg $color $@
}

polybar_fg() {
  color=$1
  shift
  echo "%{F$color}$@%{F-}"
}


function format_window() {
  icon="⚫"
  case ${1:0:1} in # first char of first arg
    ":") color $ACTIVE_WINDOW_FG "$icon" ;;
    "#") color $SELECTED_WINDOW_FG "$icon" ;;
    ".") color $EMPTY_WINDOW_FG "$icon" ;;
    "!") color $URGENT_WINDOW_FG "$icon" ;;
  esac
}

while : ; do
IFS=$'\t' read -ra tags <<< "$(hc tag_status)"
  for tag in "${tags[@]}" ; do
    echo -n "$(format_window $tag) "
  done
  echo

  hc --wait &>/dev/null
done
