#!/bin/bash

# handle ssh:// urls from firefox
#gconftool-2 -s /desktop/gnome/url-handlers/ssh/enabled --type Boolean false

[[ "$(hostname)" == "boxy" ]] && nmcli c up 'VPN plm-production' &>/dev/null
eval $(keychain -q --eval) 
if [[ "$@" =~ 'ssh://' ]] ; then
  x-terminal-emulator -bg '#223' -e sh -c "sleep 1 ;  ssh -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null ${@##ssh://}"
fi


