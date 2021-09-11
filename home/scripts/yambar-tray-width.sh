#!/bin/bash

CHARWIDTH=10 # i guess?

tray_width_px() {
    xwininfo -name panel | # trayer names its window "panel"
        grep -i width: |
        awk '{print $2}'
}

px_to_spaces() {
    spaces="$((${1:-0} / $CHARWIDTH + 1))"
    printf "% *s\n" $spaces
}

output="$(px_to_spaces `tray_width_px`)"
echo -e "padding|string|$output\n"
