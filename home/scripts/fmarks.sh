#!/bin/bash

# runs fmarks.pl.  opens result in currently selected browser

basepath=$(dirname $0)

function getWinID() {
   xprop -root | grep "_NET_ACTIVE_WINDOW(WINDOW)" | grep -v "CUT_BUFFER0" | cut -d' ' -f5
}
function getWinTitle() {
    xprop -id $(getWinID) | grep "WM_CLASS(STRING)" | sed -e 's/.*= "\([^"]*\)".*/\1/'
}

DMENU_OPTS='-l 20 -i -b -fn -*-terminus-bold-r-*-*-12 -sb "#000" -sf "#fff" -nb "#000" -nf "#888"'
. ~/.functions ; dmenu_has -s && DMENU_OPTS="$DMENU_OPTS -s 0"

browser=$($basepath/active_window.sh)
url=$($basepath/fmarks.rb "$DMENU_OPTS" "$@")

if [ "$url" != '' ] ; then
  case $browser in
    uzbl-core)  uzbl $url  ;;
    Navigator) firefox $url ;;
    chrome|luakit|midori|firefox|google-chrome|chromium)    $browser $url;;
    *) echo "dont know how to open '$url' with '$browser' " ;;
  esac
fi

