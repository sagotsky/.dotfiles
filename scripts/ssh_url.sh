#!/bin/bash

# handle ssh:// urls from firefox
#gconftool-2 -s /desktop/gnome/url-handlers/ssh/enabled --type Boolean false

[[ "$(hostname)" == "boxy" ]] && nmcli c up 'VPN patientslikeme'

if [[ "$@" =~ 'ssh://' ]] ; then
  xterm -bg '#223' -e "ssh  -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null ${@##ssh://}"
fi


