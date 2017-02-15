#!/bin/bash

# watch a git dir.  echo its branch

DIR=~/repos/plm-website

# kill leftover inotifywaits
#ps ax | grep "inotifywait.*$DIR" | sed -e 's/^ *//' | cut -f1 -d' ' | xargs kill
there-can-be-only-one.sh

git_branch() {
  git -C $DIR branch | grep \* | cut -f2 -d' '
}

git_branch

while [ $? -eq 0 ] && [ -x /usr/bin/inotifywait ] ; do
  inotifywait -r "$DIR/.git/" -e MODIFY &> /dev/null
  sleep .2

  git_branch
  sleep 1
done
