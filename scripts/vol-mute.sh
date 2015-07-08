#!/bin/sh 

amixer -c 0 sset Master,0 toggle  > /dev/null

touch /dev/snd/controlC0 

#amixer -D default sget Master,0 | grep dB | head -n1 | cut -f 7 -d " " | sed -e's/\[//g' -e 's/\]//g' > /home/sagotsky/.volinfo

# if mixer is off (muted) indicate it in volinfo
#if [ $(amixer -D default sget Master,0 | grep off | wc -c) -gt 0 ] ; then
    #echo "--%" > /home/sagotsky/.volinfo
#fi
