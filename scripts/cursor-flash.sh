#!/bin/bash

# add this to tmux/vim pane switch?
for color in green pink green pink red ; do
  urxvt-control.sh --cr $color
  sleep .01 
done 
