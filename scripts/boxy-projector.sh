#!/bin/bash

# Set boxy's screens on X11 startup
# Add this to lightdm as a display-setup-script

disper -l -d DP1 &>/dev/null && \
disper -l -d eDP1 &>/dev/null && \
  disper -e DP1,eDP1 -t left

xrandr --dpi 96
wallpaper.sh current
