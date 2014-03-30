#!/bin/sh

# gets mail counts from tbird
# https://github.com/teleshoes/thunderbird-unread-count/blob/master/unread_count-0.0.1-tb-linux.xpi
# requires inotify-wait

# kill dupes
for pid in $(pidof -x mailcount.sh) ; do
  if [ "$pid" != "$$" ] ; then
    kill -9 $pid
  fi
done

# kill leftover inotifywaits
ps ax | grep 'inotifywait.*unread-counts' | sed -e 's/^ *//' | cut -f1 -d' ' | xargs kill

COUNT=0
FILES=$(find ~/.thunderbird -name 'unread-counts' 2>/dev/null)


echo ' ' # produce some output so xmobar doesn't say updating...

while FILE=$(inotifywait -q $FILES -e ACCESS -e CLOSE_WRITE) ; do
  cat $(echo $FILE | cut -f1 -d' ') |\
    cut -f1 -d: |\
    numsum | while read num ; do
      if [ $num -gt 0 ] ; then
        #echo " [$num]"
        echo ' •'
      else 
        echo ''
      fi
    done
done


#| xargs cat | while read line ; do
  ##N=$(echo $line | cut -d: -f1)
  ##COUNT=$(( $COUNT + $N ))
  #COUNT=$(( $COUNT + $(echo $line | cut -d: -f1) ))
#done

#if [ $COUNT -gt -1 ] ; then
  #echo "[$COUNT]" 
#fi

