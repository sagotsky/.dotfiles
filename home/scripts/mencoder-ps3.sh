#!/bin/sh

exec mencoder \
  -oac lavc -ovc lavc \
  -of mpeg \
  -mpegopts format=dvd:tsaf \
  -vf softskip,scale=720:576,hqdn3d,harddup \
  -srate 48000 \
  -af lavcresample=48000:volnorm=2 \
  -ofps 25 \
  -lavcopts vcodec=mpeg2video:vrc_buf_size=1835:vrc_maxrate=5120:vstrict=0:keyint=15:vbitrate=5120:acodec=ac3:abitrate=192:autoaspect \
  "$1" -o "$2"
