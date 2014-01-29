#!/bin/bash

# music-remote.sh
#
# stupid script that wraps around music-client.sh so I can hit fewer keys
# to control my music when ssh'ed in via my iphone

declare -A CMDS
declare -A KEYS

CMDS['(p) play pause']='music-client.sh pause'
CMDS['(n) next']='music-client.sh next'
CMDS['(b) back']='music-client.sh back'
CMDS['(+) louder']='vol-up.sh'
CMDS['(-) quiet']='vol-down.sh'
CMDS['(a) rand album']='cmus-filter.sh -r -l album'
CMDS['(c) choose album']='cmus-filter.sh -l album'


# grab parentheses char
for CMD in "${!CMDS[@]}" ; do
  KEY=$(echo $CMD | sed -e 's/.*(\(.\)).*/\1/')
  [[ "${#KEY}" == "1" ]] && KEYS["$KEY"]="${CMDS[$CMD]}"
done

function menu() {
  for key in "${!CMDS[@]}" ; do
    echo "$key"
  done
  echo
}


while : ; do 
  CMD="${KEYS[$key]}"
  clear 
  music-client.sh bandsong
  menu

  [[ "$CMD" != "" ]] && $CMD
  
  read -s -N1 key 
done
