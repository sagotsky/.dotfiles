#!/bin/sh

# run a script, wrap its output for yambar

# this works, but not for the script i'd like.  colors are controlled in the yml config and their info must be typed out here.
exec "$@" | while read -r line ; do
  echo "stdout|string|$line"
  echo
done
