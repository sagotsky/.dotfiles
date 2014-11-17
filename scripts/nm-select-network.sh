#!/bin/bash


CONNECTED="$(nmcli d | grep connected | awk '{print $4}')"
ID=$( (echo $CONNECTED ; nmcli -f name -t c) | sort | uniq -c | sort -nr | cut -f8- -d' ' | dmenu -i -p "$CONNECTED" -l 6 -b)

[[ $? && "$ID" != "" ]] && nmcli c up id "$ID"
