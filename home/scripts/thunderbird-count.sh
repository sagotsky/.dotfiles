#!/bin/sh

# xmobar script for https://github.com/teleshoes/thunderbird-unread-count

there-can-be-only-one.sh

echo ''

while : ; do
  COUNT="$(cat ~/.thunderbird/*/unread-counts |
    grep '[0-9]'
    grep -v 'Local Folders' |
    cut -f1 -d: |
    head -n 1
  )"

  if [[ "$COUNT" != "0" ]] ; then
    echo ' •'
  else
    echo ''
  fi

  sleep 1m
done
