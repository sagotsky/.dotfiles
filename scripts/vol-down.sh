#!/bin/sh

#for card in `seq 0 2` ; do  amixer sset -c $card Master,0 4%- ; done

amixer -c 0 sset Master,0 4%- > /dev/null
touch /dev/snd/controlC0 
#amixer -D default sget Master,0 | grep dB | head -n1 | cut -f 7 -d " " | sed -e's/\[//g' -e 's/\]//g' > /home/sagotsky/.volinfo
