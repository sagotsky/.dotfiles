#!/bin/sh

# launch or kill trayer

CMD="trayer --align left --width 50% --edge bottom --height 64"

if [ $(pidof trayer) ] ; then
  killall trayer -u $USER;
else
  $CMD &
fi

