#!/bin/bash

# naive screen lock.  dmenu eats input until magic word is entered.

WORD='something'
LAST='somethign else'
MOUSE=$(xinput list | grep Mouse | sed -e 's/.*id=//' | cut -f 1)
TRACK=$(xinput list | grep 'Logitech Unifying Device' | sed -e 's/.*id=//' | cut -f 1)

xinput set-prop $MOUSE "Device Enabled" 0
xinput set-prop $TRACK "Device Enabled" 0

while [[ "$WORD" != "$LAST" ]] ; do 
  WORD=$(grep -v "'" /usr/share/dict/words | shuf -n 1)
  LAST=$(echo '' | dmenu -s 0 -b -p "Type '$WORD' to break toddler lock" -fn 'verdana-30-bold')
done

xinput set-prop $MOUSE "Device Enabled" 1
xinput set-prop $TRACK "Device Enabled" 1
