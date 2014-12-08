#!/bin/sh

eval $(xdotool getmouselocation --shell)
(ls & sleep 1 ) | bar -g 300x300+$X+$Y -d 
