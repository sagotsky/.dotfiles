#!/bin/sh

#for card in `seq 0 2` ; do  amixer sset -c $card Master,0 4%- ; done

amixer -D default sset Master,0 4%- > /dev/null
#amixer -D default sget Master,0 | grep dB | head -n1 | cut -f 7 -d " " | sed -e's/\[//g' -e 's/\]//g' > /home/sagotsky/.volinfo
