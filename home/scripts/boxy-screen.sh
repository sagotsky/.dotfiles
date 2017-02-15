#!/bin/bash

# Set boxy's screens on X11 startup
# Add this to lightdm as a display-setup-script

disper -l -d eDP1 &>/dev/null && \
disper -l -d DP2 &>/dev/null && \
#disper -e DP2,eDP1 -t left
disper -e DP2,eDP1 -t bottom

#xrandr --output DP2 --auto --rotate left
#xmodmap.sh
xrandr --dpi 96
wallpaper.sh current
xbacklight -set 50