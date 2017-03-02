#!/bin/bash

# music-remote.sh
#
# stupid script that wraps around music-client.sh so I can hit fewer keys
# to control my music when ssh'ed in via my iphone



MENU=''
declare -A KEYS

# read in commands.  hash comments are the message that gets displayed.  comments with a
# char in parens will have that char bound to a key.
while read line ; do
  MSG=${line##*#}
  CMD=${line%%#*}
  KEY=$(echo $MSG | sed -e 's/.*(\(.\)).*/\1/')

  [[ "${#KEY}" == "1" ]] && KEYS["$KEY"]="$CMD"
  MENU="$MENU\n$MSG"
done <<EOF
music-client.sh pause         # (p) play pause
music-client.sh next          # (n) next
music-client.sh back          # (b) back

vol-up.sh                     # (+) louder
vol-down.sh                   # (-) quiet

cmus-filter.sh --randomize --list album    # (z) rand album
cmus-filter.sh --list album       # (l) choose album
cmus-filter.sh --list artist      # (a) choose artist
cmus-filter.sh --list song        # (s) choose song

exit # (q) quit
EOF

while : ; do
  CMD="${KEYS[$key]}"
  [[ "$CMD" != "" ]] && $CMD

  clear
  music-client.sh bandsong
  echo -e $MENU

  read -s -N1 key
done
