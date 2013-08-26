#!/bin/sh

# watch a git dir.  echo its branch

DIR=/var/www/openscholar/.git

echo ' '
git --git-dir=$DIR branch | grep '\*' | cut -f 2 -d' '

while [ $? -eq 0 ] && [ -x /usr/bin/inotifywait ] ; do
  inotifywait "$DIR/HEAD" -e MODIFY &> /dev/null
  echo ' --'
  sleep .2 
  git --git-dir=$DIR branch | grep '\*' | cut -f 2 -d' '
  sleep 1
done
