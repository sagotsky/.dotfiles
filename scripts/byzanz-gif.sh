#!/bin/bash 

# Record a gif with byzanz.
# Prompts for window or screen region
# Pops up a dzen to end recording


IMAGE="/tmp/screenshot-$(date +'%F.%T').gif"
. "$HOME/.functions" # get byzanz-dimensions

function dzen() {
  echo $@ | dzen2 $DZEN_OPTS -p -e 'button1=exit;button2=exit;button3=exit' -ta l
}
export -f dzen

dzen 'gif -> imgur: Click a window.' &
DIMS=$(byzanz-dimensions) 
kill $(pidof dzen2 | rev | cut -f 1 -d ' ' | rev) &>/dev/null
  
byzanz-record -d 60 $DIMS "$IMAGE" --exec "bash -c dzen\ 'Recording window... click to exit'"
echo $IMAGE



