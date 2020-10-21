#!/bin/sh

#TODO: stdin vs args?
export DISPLAY="${DISPLAY:-$(cat ~/.display)}"
IFS=''
while read line ; do
   echo "$line"
done | xclip -i -selection clipboard
xclip -o -selection clipboard
