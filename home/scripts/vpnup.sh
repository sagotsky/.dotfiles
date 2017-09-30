#!/bin/sh

VPN="openvpn-client@plm"
if [[ $(systemctl is-active $VPN &>/dev/null) ]] ; then
  :
else
  echo 'Gimme vpn!'
  sudo systemctl start $VPN
  sleep 5
fi
