#!/bin/bash

# hidpi laptop screen
xrandr \
  --output DP-4 \
    --primary \
    --dpi 160 \
    --left-of DP-3.2 \
  --output DP-3.2 \
    --scale 2x2     # scale external display


# geography
# xrandr --output DP-4 --below DP-3.2
xbacklight -set 75
