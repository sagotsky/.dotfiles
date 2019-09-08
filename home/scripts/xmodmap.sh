#!/bin/sh
caps_unlock.sh
setxkbmap -option caps:super
setxkbmap -option terminate:ctrl_alt_bksp
xmodmap ~/.xmodmap 2>/dev/null
xset s 300 # 5m screensaver
