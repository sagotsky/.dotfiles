#!/bin/sh

file=$1
histdir="$HOME/.dmenu-hist/"
bucket="$histdir/$file"
shift

if [ ! -d "$histdir" ] ; then mkdir "$histdir" ; fi

if [ ! -f "$bucket" ] ; then touch "$bucket" ; fi

IFS=$'\n' stdin="$(cat)"
sel=$(echo -e "$stdin\n$(grep . $bucket)" | sort | uniq -c | sort -bnr | sed -e 's/.*[0-9]\+ //' | dmenu $@)

if [ "$?" -eq "0" ] ; then 
  echo "$sel" >> "$bucket"
  echo "$sel"
else
  exit 1
fi
