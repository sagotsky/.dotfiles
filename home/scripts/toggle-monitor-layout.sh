#!/bin/bash

xmodmap ~/.xmodmap
least_recently_used_layout="$( ls -t ~/.screenlayout/*.sh | tail -n 1 )"
echo $least_recently_used_layout
echo
$least_recently_used_layout
touch -m $least_recently_used_layout
killall yambar
yambar &
