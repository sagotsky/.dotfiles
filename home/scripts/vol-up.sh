#!/bin/sh

N=${1:-4}

amixer -D pulse -c 0 sset Master $N%+ > /dev/null
amixer -D pulse -c 3 sset PCM $N%+ > /dev/null
