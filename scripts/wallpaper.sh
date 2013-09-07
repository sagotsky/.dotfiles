#!/bin/sh

# wallpaper switcher
IMG=$( find ~/.wallpaper/ | shuf -n1 )
feh --bg-fill --no-xinerama "$IMG"

notify-send mod-b "$IMG" &
