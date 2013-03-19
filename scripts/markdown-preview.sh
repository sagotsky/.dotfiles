#!/bin/sh

# Preview markdown file each time it's written to

if [ -e "$@" ] ; then
  FILE="$@"
else 
  FILE="${PWD}/$@"
fi

OUT="/tmp/markdown_temp.html"
while [ -x /usr/bin/inotifywait ] && [ -x /usr/bin/markdown ] ; do 

  markdown $FILE >$OUT
  uzbl $OUT &>/dev/null &
  PID=$!
  inotifywait "$FILE" -e MODIFY &>/dev/null
  kill $PID &>/dev/null
done
