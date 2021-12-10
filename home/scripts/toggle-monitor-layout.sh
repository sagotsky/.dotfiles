#!/bin/bash -xe

least_recently_used_layout="$( ls -t ~/.screenlayout/*.sh | tail -n 1 )"
notify-send "layout:" "$least_recently_used_layout"

$least_recently_used_layout
touch -m $least_recently_used_layout

killall yambar
sleep 0.5

setxkbmap -option terminate:ctrl_alt_bksp
xmodmap "$HOME/.xmodmap"

yambar &
[ -f ~/.xsession.d/picom.sh ] && ~/.xsession.d/picom.sh &
nitrogen --restore
