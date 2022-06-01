#!/bin/sh

pactl set-sink-volume 1 +4% > /dev/null
# amixer sset Master 4%+ unmute > /dev/null
touch /dev/snd/controlC0
