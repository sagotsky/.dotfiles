#!/bin/bash

# just a wrapper for my fav dmenu settings

ARGS=" -b -m 0 -sb \'\#cfb000\' " #-sf '\#000' -nf '\#fff' -nb '4a525a' -fn '-*-fixed-bold-r-*-*-14-*-*-*-*-*-*-*' "

#exe=`dmenu_path | dmenu $ARGS ${1+"$@"}` && exec $exe
exe=$(dmenu_path | dmenu $ARGS) && $exe
