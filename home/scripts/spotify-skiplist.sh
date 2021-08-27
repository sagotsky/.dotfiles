#!/usr/bin/env bash

CONFIG="$HOME/.spotify-skiplist"

check_dependencies() {
   which spotifyctl &>/dev/null || abort "You must install playerctl"
}

check_config() {
  [[ -f ~/.spotify-skiplist ]] || abort "$CONFIG does not exist"
}

abort() {
  echo $@
  exit 1
}

usage() {
  echo "Run spotify-skiplist.sh as a daemon.  Artists in $CONFIG will automatically be skipped."
  echo "E.g. \`echo Nickelback >> $CONFIG\`"
}

if [[ "$#" != "0" ]] ; then
   usage
   exit
fi

check_dependencies
check_config

spotifyctl metadata artist --follow 2>/dev/null | while read artist ; do
  if grep "${artist}" $CONFIG ; then
    echo "Skipping ${artist}"
    spotifyctl next
  fi
done
