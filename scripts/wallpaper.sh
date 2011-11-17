#!/bin/sh

# wallpaper switcher

feh --bg-fill "$( find ~/.wallpaper/ | shuf -n1 )"
