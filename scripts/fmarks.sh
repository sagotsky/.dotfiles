#!/bin/bash

# runs fmarks.pl.  opens result in currently selected browser

function getWinID() {
   xprop -root | grep "_NET_ACTIVE_WINDOW(WINDOW)" | grep -v "CUT_BUFFER0" | cut -d' ' -f5
}
function getWinTitle() {
    xprop -id $(getWinID) | grep "WM_CLASS(STRING)" | sed -e 's/.*= "\([^"]*\)".*/\1/'
}


browser=$(active_window.sh)
url=$(fmarks.pl)

case $browser in
  uzbl-core)  uzbl $url  ;;
  Navigator) firefox $url ;;
  firefox|google-chrome)    $browser $url;;
esac

