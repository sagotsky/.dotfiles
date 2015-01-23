#!/bin/sh

function signal() {
  grep wlan /proc/net/wireless | awk '{print $3*100/70}' | cut -f 1 -d'.'
}

function essid() {
  nmcli d | grep connected | grep wlan | awk '{print $4}'
}

[[ "$(signal)" -lt 60 ]] && echo "<fc=red><icon=wifi.xbm/> $(signal)%</fc>" || echo ''

