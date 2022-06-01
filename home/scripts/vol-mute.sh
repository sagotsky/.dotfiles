#!/bin/sh

# amixer sset Master  toggle  > /dev/null
pactl set-sink-mute toggle >/dev/null

touch /dev/snd/controlC0
