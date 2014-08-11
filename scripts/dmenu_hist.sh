#!/bin/sh

file=$1
menu_cmd='dmenu'
[[ "$(basename $0)" == 'slmenu_hist.sh' ]] && menu_cmd='slmenu'  # symlink to this file to use slmenu instead of dmenu
histdir="$HOME/.dmenu-hist/"
bucket="$histdir/$file"
shift

if [ ! -d "$histdir" ] ; then mkdir "$histdir" ; fi

if [ ! -f "$bucket" ] ; then touch "$bucket" ; fi

IFS=$'\n' stdin="$(cat)"
sel=$(echo -e "$stdin\n$(grep . $bucket)" | sort | uniq -c | sort -bnr | sed -e 's/.*[0-9]\+ //' | $menu_cmd $@)

if [ "$?" -eq "0" ] ; then 
  echo "$sel" >> "$bucket"
  echo "$sel"
else
  exit 1
fi
