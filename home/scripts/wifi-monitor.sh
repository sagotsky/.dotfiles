#!/bin/bash

function signal() {
  tail -n 1 /proc/net/wireless | awk '{print $3*100/70}' | cut -f 1 -d'.'
}

function essid() {
  iwconfig 2>/dev/null | grep ESSID | cut -f 2 -d\"
}

[[ "$(signal)" -lt 60 ]] && echo "<fc=red><icon=wifi.xbm/> $(signal)%</fc>" || echo ''

