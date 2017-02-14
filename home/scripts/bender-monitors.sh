#!/bin/bash

EXTERN="DP-3.2"
EXTERN_SCALE="2x2"
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


xbacklight -set 75
