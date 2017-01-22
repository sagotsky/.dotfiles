#!/bin/bash

if [[ `which slmenu` ]] ; then
  opt=$(git branch -a | slmenu -l 10 | tr -d '*')
  [[ "$opt" != '' ]] && git co $opt
else
  OPTS=$(git branch | tr -d '*' | sort -n | xargs)
  select opt in $OPTS; do
    if [ $opt ] ; then
      git checkout $opt
    fi
    break;
  done
fi
