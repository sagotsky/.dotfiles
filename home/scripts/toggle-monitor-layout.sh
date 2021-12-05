#!/bin/bash

least_recently_used_layout="$( ls -t ~/.screenlayout/*.sh | tail -n 1 )"
notify-send "layout:" "$least_recently_used_layout"

$least_recently_used_layout
touch -m $least_recently_used_layout
sleep 0.1

xmodmap ~/.xmodmap
killall yambar
yambar &
[ -f ~/.xsession.d/picom.sh ] && ~/.xsession.d/picom.sh &
nitrogen --restore
setxkbmap -option terminate:ctrl_alt_bksp
