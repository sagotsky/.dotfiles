#!/bin/bash

# runs fmarks.pl.  opens result in currently selected browser

basepath=$(dirname $0)

function getWinID() {
   xprop -root | grep "_NET_ACTIVE_WINDOW(WINDOW)" | grep -v "CUT_BUFFER0" | cut -d' ' -f5
}
function getWinTitle() {
    xprop -id $(getWinID) | grep "WM_CLASS(STRING)" | sed -e 's/.*= "\([^"]*\)".*/\1/'
}

browser=$($basepath/active_window.sh)
url=$($basepath/fmarks.pl)

if [ "$url" != '' ] ; then
  case $browser in
    uzbl-core)  uzbl $url  ;;
    Navigator) firefox $url ;;
    firefox|google-chrome)    $browser $url;;
    *) echo "dont know how to open '$url' with '$browser' " ;;
  esac
fi

