#!/bin/sh

DIR=${1:-$PWD}
cd $DIR
git branch |
  grep '*' |
  cut -f 2 -d ' '
