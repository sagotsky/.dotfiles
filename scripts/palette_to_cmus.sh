#!/bin/sh

# get all non blacks, sorted by freq desc, and removes color codes.  
# @TODO make a pun about the blacklist.
COLORS="$(
  ( 
    while read COLOR ; do
      colortrans.py $COLOR
    done < "$HOME/.palette.txt" 
  ) | cut -f 7 -d' ' |
  sort |
  uniq -c | 
  sort -n |
  cat -v |
  sed -e 's/.*m//' |
  grep -v '^\(232\|233\|234\|00\|16\)$'
)"

cmus-remote -C "set color_titleline_fg=$(echo "$COLORS" | tail -n 5 | shuf -n 1)"
cmus-remote -C "set color_win_cur=$(echo "$COLORS" | tail -n 5 | shuf -n 1)"
