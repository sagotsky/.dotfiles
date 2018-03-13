#!/bin/bash

source ~/.functions # delay()

VPN="openvpn-client@plm"
if [[ "$(systemctl is-active $VPN)" == 'active' ]] ; then
  echo 'got vpn!'
else
  echo 'Gimme vpn!'
  sudo systemctl start $VPN
  delay 5
fi
