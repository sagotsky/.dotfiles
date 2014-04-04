#!/bin/bash

connected() {
  nm-tool | 
    grep '  State: *connected' -B 3 |
    head -n 1 |
    sed -e 's/.*\[\(.*\)\].*/\1/'
}

CURRENT=$(connected)


while : ; do
  LAST="$CURRENT"
  CURRENT=$(connected)

  case "$CURRENT" in 
    '') echo 'Net = ☹' ;;
    "$LAST") echo ''    ;;
    *) echo "$CURRENT" ;;
  esac 
  sleep 1m
done
