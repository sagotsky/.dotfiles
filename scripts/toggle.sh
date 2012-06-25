#!/bin/sh

# kills arg or starts arg
# useful for toggling display of a panel

kill $(pidof $1) &>/dev/null || exec "$@" &
