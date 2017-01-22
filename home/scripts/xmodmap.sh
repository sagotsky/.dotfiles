#!/bin/sh
setxkbmap -option caps:super
setxkbmap -option terminate:ctrl_alt_bksp
xmodmap ~/.xmodmap
xset s 300 # 5m screensaver
