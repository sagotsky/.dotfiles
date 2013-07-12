#!/bin/sh

# wallpaper switcher

feh --bg-fill --no-xinerama "$( find ~/.wallpaper/ | shuf -n1 )"
