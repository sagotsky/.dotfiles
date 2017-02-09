#!/bin/bash

if [[ "$#" == "0" ]] ; then
  echo "Point this script to a saved solano csv report"
  exit 1
fi

if  ! which solano &>/dev/null  ; then
  echo "please install solano gem"
  echo "gem install solano"
  echo "solano login"
  exit 1
fi

cat $1 |
  cut -f 4 -d\" |
  grep '\.' |
  xargs -n1 solano describe |
  grep '^test/' |
  cut -f 1 -d' ' |
  sort |
  uniq -c |
  sort -n
