#!/bin/sh

# wallpaper switcher
IMG=$( find ~/.wallpaper/ | shuf -n1 )
notify-send mod-b "$IMG" &
feh --bg-fill --no-xinerama "$IMG"

