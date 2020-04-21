#!/usr/bin/env bash

function border_bounce() {
  while : ; do
   for n in `seq 300` `seq 99 -1 1` ; do
     herbstclient set frame_border_width $n
   done
  done
}

function party_borders() {
  while : ; do
    for color in yellow red green white black blue pink magenta; do
      herbstclient set frame_border_normal_color  $color
      herbstclient set frame_border_active_color  $color
      sleep .1
    done
  done
}

function smooth_borders() {
  while : ; do
    for color in `seq 0 9` a b c d e f e d c b a `seq 9 -1 0`; do
      herbstclient set frame_border_active_color "#$color$color$color"
      herbstclient set frame_border_normal_color "#$color$color$color"
      sleep .01
    done
  done
}
