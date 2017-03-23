#!/bin/bash

# should this run before or after xinitrc exports scaling vars?

EXTERN="DP-3.2"
# EXTERN_SCALE="2x2" # headache!
EXTERN_SCALE="1x1"
EXTERN_ON='auto'

INTERN="DP-4"
INTERN_POS="--left-of $EXTERN"
INTERN_DPI="160"

MODE=$1
case $MODE in
  solo)
    EXTERN_ON='off'
    ;;
  clone)
    # todo: find a better resolution or disable scaling.
    INTERN_POS=''
    ;;
  startx)
    # initialize with this scaling, then scale back to default
    EXTERN_SCALE='2x2'
    ;;
esac

xrandr \
  --output $INTERN\
    --primary \
    --dpi $INTERN_DPI\
    $INTERN_POS \
    --auto \
  --output $EXTERN \
    --$EXTERN_ON \
    --scale $EXTERN_SCALE


nitrogen --restore
xbacklight -set 75
