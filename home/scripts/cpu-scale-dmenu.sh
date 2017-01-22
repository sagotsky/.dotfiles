#!/bin/bash

function current() {
  cpufreq-info | 
    grep current\ CPU | 
    head -n 1 | 
    sed -e 's/current CPU frequency is //'
}

FREQS="$(echo {4,3,2,1}.0ghz)"
FREQ="$(
  echo "$FREQS" | xargs -n 1 echo |
  dmenu -s 0 -p "$(current)"
)"

# passing stuff to sudo seems bad here.  won't work with dmenu.  need to check if the input from dmenu is in array it started with.
if [[ "$?" == "0" && $FREQS == *"$FREQ"* ]] ; then
  \sudo cpufreq-set --max $FREQ 
  sleep 0.8
  notify-send CPU "$(current)" -u low
fi 
