#!/usr/bin/env bash

external_screen="DP-1-1.1"
internal_screen="eDP-1"

notify-send "docking" -h string:fgcolor:#ffffff -h string:bgcolor:#00b373

~/.screenlayout/vertical-2.sh
xmodmap ~/.xmodmap

# `xrandr -q` lags for 3s.  this tries to skip the query by offering a fallback.
xrandr --output "$external_screen" --primary ||                     # try external
    xrandr --output "$internal_screen" --primary       # fallback to laptop

killall yambar ; yambar &
