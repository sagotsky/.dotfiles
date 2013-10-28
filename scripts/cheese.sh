#!/bin/bash

# Cheese - manipulate your mouse
#
# Dragging across 3200 pixels sucks.  Cheese uses xdotool to 
# move the mouse to a desired location.  Tilers rejoice!


function jiggle {
  eval $(xdotool getmouselocation --shell)
  for n in `seq 40 -3 1` ; do
    # we jigglin'?
    if [[ "$n" -gt 0 ]] ; then
      # we jiggling!
      RX=$RANDOM
      RY=$RANDOM
      xdotool mousemove $(( X + $RX%$n - n/2 )) $(( Y + RY%$n - n/2 ))
      sleep .01
    fi
  done


}

eval $(xdotool getwindowgeometry --shell $(xdotool getactivewindow))
xdotool mousemove --window $(xdotool getactivewindow) $(( WIDTH / 2 - 8 )) $(( HEIGHT / 2 - 16  ))
jiggle
