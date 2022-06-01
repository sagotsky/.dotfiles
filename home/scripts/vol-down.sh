#!/bin/sh


# amixer sset Master 4%- > /dev/null
pactl set-sink-volume 1 -4% > /dev/null
touch /dev/snd/controlC0
