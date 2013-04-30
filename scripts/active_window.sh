#!/bin/bash

# gets name of active window
# opens appropriate manpage term

# should handle gui windows, programs running in terms, and even programs running in screen

function getWinID() {
   xprop -root | grep "_NET_ACTIVE_WINDOW(WINDOW)" | grep -v "CUT_BUFFER0" | cut -d' ' -f5
}
function getWinTitle() {
    xprop -id $(getWinID) | grep "WM_CLASS(STRING)" | sed -e 's/.*= "\([^"]*\)".*/\1/'
}

echo $(getWinTitle)

