#!/bin/sh

# wallpaper switcher

feh --bg-tile "$( find ~/.wallpaper/ | shuf -n1 )"
