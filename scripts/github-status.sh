#!/bin/bash

# github status api for xmobar.  
STATUS=$(curl https://status.github.com/api/status.json 2>/dev/null | jshon -e status | tr -d '"')

# case!  be a bash array!
case "$STATUS" in
  'minor') COLOR="black,goldenrod" ;;
  'major') COLOR="white,red" ;;
  'good') COLOR='';;
  *) COLOR="white" ;;
esac

if [ "$STATUS" != '' -a "$COLOR" != '' ] ; then
  echo "<fc=$COLOR>Github status: $STATUS</fc> |"
fi

# known statuses: major, minor, good
