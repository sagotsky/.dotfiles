#!/bin/bash

# kills arg or starts arg
# useful for toggling display of a panel

# TODO: per user
kill $(pidof $1) &>/dev/null || exec "$@" &
