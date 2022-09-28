#!/bin/sh

amixer sset Master  toggle  > /dev/null
# pactl set-sink-mute $(pacmd-default-sink) toggle >/dev/null

# touch /dev/snd/controlC0
