#!/usr/bin/env bash

# tiny daemon that forever echos title of active window

window_id=""
while read -r window_id_line ; do
    new_window_id="$(echo \"$window_id_line\" | cut -d\# -f 2)"
    if [[ "$new_window_id" != "$window_id" ]] ; then
        window_id="$new_window_id"
        xdotool getwindowname "$window_id"
    fi
done < <(xprop -root -spy -len 4 | grep --line-buffered "^_NET_ACTIVE_WINDOW")
