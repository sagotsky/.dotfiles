#!/bin/sh

# gets mail counts from tbird
# https://github.com/teleshoes/thunderbird-unread-count/blob/master/unread_count-0.0.1-tb-linux.xpi

COUNT=0
find ~/.thunderbird -name unread-counts | xargs cat | while read line ; do
  #N=$(echo $line | cut -d: -f1)
  #COUNT=$(( $COUNT + $N ))
  COUNT=$(( $COUNT + $(echo $line | cut -d: -f1) ))
done

if [ $COUNT -gt -1 ] ; then
  echo "[$COUNT]" 
fi

