#!/bin/bash

# Prints volume changes to stdout.  
# Depends on inotify-tools package

SOUND_DEV="/dev/snd/controlC0"

# there can be only one
pidof -x $0 | sed -e "s/$$//" | xargs kill 2>/dev/null    

# this should grab the field with the percentage.  maybe break on " " and grep line with %?
volume() {
	amixer -D default sget Master,0 \
	  | grep % \
	  | sed -e 's/.*\[\(.\{1,3\}%\)\].*/\1/'  \
          | head -n 1
}


volume_n() {
    volume | tr -d "%" 
}

echo $(volume_n)%

# loop only runs when inotify didn't fail (not present on all systems)
# and when parent process is xmobar.  this _should_ ensure script quits after xmonad resets

while [ $? -eq 0 ] &&  [ -x /usr/bin/inotifywait ] ; 
#while [ $? -eq 0 ] && [[  $(ps p $PPID | grep xmobar) ]] && [ -x /usr/bin/inotifywait ] ; 
do
	inotifywait $SOUND_DEV -e ACCESS -e CLOSE_WRITE > /dev/null 2>/dev/null
    sleep .01
    VOL=$(volume_n)
    COLORS="white gray dimgray"
    [[ "$VOL" -gt 60 ]] && COLORS="yellow white gray"
    [[ "$VOL" -gt 90 ]] && COLORS="yellow orange red"

    for FC in $COLORS ; do echo "<fc=$FC>$VOL%</fc>" ; sleep .05 ; done
done

