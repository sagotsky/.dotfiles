#!/bin/bash

# handle ssh:// urls from firefox
#gconftool-2 -s /desktop/gnome/url-handlers/ssh/enabled --type Boolean false

if [[ "$@" =~ 'ssh://' ]] ; then
  xterm -bg '#223' -e "ssh ${@##ssh://}"
fi

