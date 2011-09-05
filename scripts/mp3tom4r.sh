#!/bin/sh

# makes iphone ringtone out of 1st 30 sec mp3 file

TMP=$(mktemp)
MP3=$(mktemp)  # mplayer is choking on spaces...
cp "$@" $MP3

mplayer "$MP3" -endpos 30 -ao pcm:file="$TMP"
faac "$TMP" -o "${@/.mp3/}.m4r" -w -b 128     # yay, extension stripping

rm $TMP $MP3

