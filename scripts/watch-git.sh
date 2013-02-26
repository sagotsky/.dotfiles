#!/bin/sh

# watch a git dir.  echo its branch

DIR=/var/www/scholar7/profiles/openscholar/.git

while [ $? -eq 0 ] && [ -x /usr/bin/inotifywait ] ; do
  inotifywait "$DIR/HEAD" -e MODIFY &> /dev/null
  git --git-dir=$DIR branch | grep '\*' | cut -f 2 -d' '
  sleep 1
done
