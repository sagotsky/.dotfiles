#!/bin/sh

pkill stalonetray
pkill trayer

# launch a tray.
echo

sleep 0.1 # make sure yambar starts first so we can draw on tpo of it

# trayer-srg fork!
trayer \
    --edge top \
    --tint 0x24242400 \
    --align right \
    --height 20 \
    --expand false  \
    --transparent true \
    --alpha 0 \
    --width 20  \
    --monitor primary \
    --widthtype request
    # &>/dev/null
