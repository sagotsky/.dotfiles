#!/bin/bash

OPTS=$(git branch | tr -d '*' | xargs)
select opt in $OPTS; do
  if [ $opt ] ; then
    git checkout $opt
  fi
  break;
done

