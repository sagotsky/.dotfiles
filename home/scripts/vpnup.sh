#!/bin/bash

source ~/.functions # delay()

for vpn in plm aws-preprod ; do
  service="openvpn-client@$vpn"
  if [[ "$(systemctl is-active $service)" == 'active' ]] ; then
    echo 'got vpn!'
  else
    echo 'Gimme vpn!'
    sudo systemctl start $service
    delay 5
  fi
done
