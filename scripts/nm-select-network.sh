#!/bin/bash


CONNECTED="$(nm-tool | grep '  State: *connected' -B 3 | head -n 1 | sed -e 's/.*\[\(.*\)\].*/\1/')"
ID=$( (echo $CONNECTED ; nmcli -f name -t c) | sort | uniq -c | sort -nr | cut -f8- -d' ' | dmenu -p "$CONNECTED" -l 6 -b)

[[ $? && "$ID" != "" ]] && nmcli c up id "$ID"
