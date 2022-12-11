#!/bin/sh

N=${1:-4}

# todo: map /proc/asound/cards on name -> control
amixer -D pulse -c 0 sset Master $N%- > /dev/null
amixer -D pulse -c 3 sset PCM $N%- > /dev/null
