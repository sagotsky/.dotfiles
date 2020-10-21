#!/bin/sh

DEVICE="wlp61s0"

lan_ip() {
   ip addr |
      grep $DEVICE |
      grep inet |
      sed -e 's/.*inet//' -e 's/\/.*//'
}

wan_ip() {
   curl -s -m 0.5 http://icanhazip.com
}

essid() {
   iwconfig $DEVICE |
      grep ESSID |
      cut -f2 -d\"
}

notify-send WIFI "\t$(essid)\t $(lan_ip)\t $(wan_ip)"
