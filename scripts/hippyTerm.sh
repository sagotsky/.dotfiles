#!/bin/bash

# random background colors for an xterm
# colors are not strictly random.  1/2/3 channel is selected first, then colors picked within that.
# background color gets averaged to determine if fg should be white or black

COLORS=3
MAX=255

# args (for future versions)
# -n new term window.  otherwise just recolor with xtermcontrol (unless it doesn't exist)
# -rRgGbB.  color masks.  lowercase sets to 00.  upper to FF.  
# color masks will probably take place in proccolors

#opens a terminal
function term() {
    xterm -bg "$1" -fg "$2" &
}
# converts decimal $1 to any base $2
function dec2base() {
    if [ $# -ne 2 ] ; then
        echo -e "\nUsage: $0 number base.\n\n"
    fi

    echo "$1 $2 o p " | dc
}

# shortcut for converting to hex
function hex() {
    dec2base $1 16
}

# random number from 1 to $1
function rand() {
    seq 1 $1 | shuf -n 1
}

# returns a color value between 0 and 255
function color() {
    echo $(( $(rand $(($MAX+1))) -1))
}

# returns black or white for a single channel
function bw() {
    [[ $(rand 2) == 1 ]] && echo "00" || echo "$MAX"
}

# how many color channels have a non b/w value
CHANNELS=$(rand $COLORS)

#  fetches all color values
function getColors() {
    for i in $(seq $COLORS) ; do
        if [[ $i -le $CHANNELS ]] ; then
            color
        else
            bw
        fi
    done
}

# turn colors to hex, concatenate them.  figure out foreground
function procColors() {
    SUM=0
    BG="#"
    while read line ; do
        SUM=$(( $SUM + $line ))
        C=$(hex $line)
        [[ $(echo $C | wc -c) -lt 3 ]] && C="0$C"
        BG="$BG$C"
    done

    if [[ $(( $SUM/$COLORS )) -lt 128 ]] ; then
        FG="white" 
    else
        FG="black"
    fi

    echo $BG $FG
}

term $(getColors | shuf | procColors)

