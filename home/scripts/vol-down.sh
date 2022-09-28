#!/bin/sh


amixer sset Master 4%- > /dev/null
# pactl set-sink-volume $(pacmd-default-sink) -4% > /dev/null
# touch /dev/snd/controlC0
