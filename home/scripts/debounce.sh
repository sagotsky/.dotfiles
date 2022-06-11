#!/bin/bash

# buffers line output, eating duplicate lines
last_line=""
while read -r line ; do
    if [[ "$line" != "$last_line" ]] ; then
        echo "$line"
        last_line="$line"
    fi
done
