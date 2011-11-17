#!/bin/bash


select opt in $(git branch | tr -d '*' | xargs) ; do
  git checkout $opt
  break;
done

