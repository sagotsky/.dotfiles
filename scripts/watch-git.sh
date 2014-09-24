#!/bin/bash

# watch a git dir.  echo its branch

DIR=~/repos/open-pro/.git

# kill leftover inotifywaits
ps ax | grep "inotifywait.*$DIR" | sed -e 's/^ *//' | cut -f1 -d' ' | xargs kill

echo " $(git --git-dir=$DIR branch | grep '\*' | cut -f 2 -d' ') "

while [ $? -eq 0 ] && [ -x /usr/bin/inotifywait ] ; do
  inotifywait "$DIR/HEAD" -e MODIFY &> /dev/null
  echo ' --'
  sleep .2 
  git --git-dir=$DIR branch | grep '\*' | cut -f 2 -d' '
  sleep 1
done
