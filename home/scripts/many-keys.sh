#!/bin/bash

# Sends key input to multiple windows.  Combine with vimperator to make QA suck less.

one=$(xwininfo | grep id: | cut -f4 -d' ')
#flash-active-window.sh
two=$(xwininfo | grep id: | cut -f4 -d' ')
#LAG=50  # needs tuning

echo $one $two 
while : ; do
  read -r -s -N1 key
  [[ "$key" == '' ]] && key='\r'
  echo "key: '$key'"
  xdotool type --window $one "$key" &>/dev/null
  xdotool type --window $two "$key" &>/dev/null
  #     wmctrl -i -a 0x02200003   #forcibly set focus in window running script
done
