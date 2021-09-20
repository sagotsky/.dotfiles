#!/bin/sh

window_id="$(xdotool getwindowfocus)"
xprop -id $window_id -format TAG_INVERT 8c \
    -set TAG_INVERT $(\
    xprop -id $window_id 8c TAG_INVERT \
    | sed -e 's/.*= 1.*/0/' -e 's/.*= 0.*/1/'  -e 's/.*not found.*/1/'\
)
