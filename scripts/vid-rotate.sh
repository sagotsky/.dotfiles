#!/bin/sh

# Rotate a video by 90 degrees

for file in $@ ; do
  base=$(echo ${file%.*})
  mencoder -oac copy -ovc lavc -vf  rotate=1 "$file" -o ${file%.*}_90.avi 
done
