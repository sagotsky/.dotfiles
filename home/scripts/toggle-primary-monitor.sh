#!/usr/bin/env bash

# autorandr --load horizontal

# works for getting off secondary.  how about reconnecting?
NEXT_PRIMARY=$(xrandr -q | grep ' connected' | grep -v 'primary' | cut -f 1 -d' ')
if [[ "$NEXT_PRIMARY" == "" ]] ; then
  # just use the connected screen.  it can stay primary.
  NEXT_PRIMARY=$(xrandr -q | grep ' connected' |  cut -f 1 -d' ')
fi

# will this drop the other screen?  or just switch primary?
xrandr --output $NEXT_PRIMARY --primary
# xmonad --restart

sleep 1 ; nitrogen --restore
