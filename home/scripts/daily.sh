#!/bin/bash

echo "$@" >> /tmp/daily.log
# runs something.  aborts if it's already run today.
today="$(date +%Y%m%d)"
cmd_fingerprint="$(echo $@ | md5)"
today_file="/tmp/daily.sh-${cmd_fingerprint}-${today}"

if [[ -f "$today_file" ]] ; then
    exit 0
else
    "$@"
    touch $today_file
fi
