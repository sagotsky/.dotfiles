#!/bin/zsh

# watch a git dir.  echo its branch

DIR=~/repos/ore/

# kill leftover inotifywaits
#ps ax | grep "inotifywait.*$DIR" | sed -e 's/^ *//' | cut -f1 -d' ' | xargs kill

echo $(git-super-status.sh $DIR)

while [ $? -eq 0 ] && [ -x /usr/bin/inotifywait ] ; do
  inotifywait -r "$DIR/.git/" -e MODIFY &> /dev/null
  sleep .2 
  echo $(git-super-status.sh $DIR)
  sleep 1
done
