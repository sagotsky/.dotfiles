#!/bin/bash

# sets dunst colors from env vars.  that's all.

pkill -x dunst

eval `xrdb_dump.sh`

dunst \
  -lf "$xrdb_color5" \
  -lb "$xrdb_background" \
  -nf "$xrdb_color6" \
  -nb "$xrdb_background" \
  -cf "$xrdb_urgent" \
  -cb "$xrdb_background"
