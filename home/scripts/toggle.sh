#!/bin/bash

# kills arg or starts arg
# useful for toggling display of a panel

# TODO: per user
pid="$(pidof $1)"
if [[ "$pid" != "" ]] ; then
    kill $pid &>/dev/null
else
    exec "$@"
fi
# kill $(pidof $1) &>/dev/null || exec "$@" &
