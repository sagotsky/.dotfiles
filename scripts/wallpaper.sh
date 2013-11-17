#!/bin/sh

# wallpaper switcher
IMG=$( find ~/.wallpaper/ | shuf -n1 )
notify-send M-S-b "$IMG" &
feh --bg-fill --no-xinerama "$IMG"


# todo: compare \d{4}x\d{4} with xwininfo -root.  ignore smaller wallapeprs.  intelligently position big ones?
# this is probably slow, could it be cached somewhere?
