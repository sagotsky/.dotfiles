#!/bin/sh

#xterm -fa droidsansmono-16 -bg '#1c1c1c' -fg white -class cmus -e 'TERM=xterm-256color cmus' &
urxvt -fn "xft:Droid Sans Mono-16" -bd black -b 2 -bg '#1c1c1c' -fg '#fff' +sb -name cmus -e sh -c 'rxvt-background.sh & cmus' &

sleep 1
cmus-remote -C add Music/
cmus-filter.sh --list album --query "$(shuf -n 1 < ~/.album-bookmarks)"
sleep 1
cmus-remote --play
cmus-remote --stop
