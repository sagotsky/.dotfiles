#!/bin/sh

#for card in `seq 0 2` ; do 
	#amixer sset -c $card PCM,0 60% unmute
	#amixer sset -c $card Master,0 4%+ unmute
	#done


#amixer -c 0 sget Master,0 | grep dB | head -n1 | cut -f 7 -d " " | sed -e's/\[//g' -e 's/\]//g' > /home/sagotsky/.volinfo

#amixer -D default sset PCM,0 100% unmute > /dev/null
amixer -D default sset Master,0 4%+ unmute > /dev/null
touch /dev/snd/controlC0 
#amixer -D default sget Master,0 | grep dB | head -n 1 | cut -f 7 -d " "| sed -e's/\[//g' -e 's/\]//g' > /home/sagotsky/.volinfo

