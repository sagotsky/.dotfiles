#!/bin/bash

# echo "event_command = $PWD/scripts/pianobar-event.sh" >> $PWD/.config/pianobar/config
OUT="/tmp/pianobar-event"
echo '' >> $OUT
echo $@ >> $OUT
while read line ; do echo $line >> $OUT ; done
