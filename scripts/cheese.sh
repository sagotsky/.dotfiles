#!/bin/bash

# Cheese - manipulate your mouse
#
# Dragging across 3200 pixels sucks.  Cheese uses xdotool to 
# move the mouse to a desired location.  Tilers rejoice!

OFFSET=10

# mouse follows 
xdotool mousemove --window `xdotool getactivewindow` $(( $OFFSET-1 )) $(( OFFSET-1 ))
sleep .1
xdotool mousemove --window `xdotool getactivewindow` $OFFSET $OFFSET
