#!/bin/sh

wap="$(netctl list | dmenu | tr -d '*')"
if [[ "$wap" == "" ]] ; then exit 1 ; fi

sudo /usr/bin/netctl stop-all
sudo /usr/bin/netctl start $wap
