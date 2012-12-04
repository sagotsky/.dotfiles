#!/bin/sh

TARGET=$1

drush $1 ms |
  cut -f 2 -d' ' |
  grep . |
  grep -v "\(Table\|Group\|vocab\|comment\|User\|Theme\|Vocab\|Boxes\|Variable\|Menu\|Context\)" |
  tr [:upper:] [:lower:] |
  while read table ; do
    drush $1 sqlq "SELECT distinct message FROM migrate_message_$table " | grep -i file
  done

