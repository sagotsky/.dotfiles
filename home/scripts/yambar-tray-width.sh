#!/bin/bash

CHARWIDTH=20 # i guess?

tray_width_px() {
    wmctrl -l -G |
        grep "b[p]anelb" |   # trayer names its window "panel"
        awk '{print $5}'
}

px_to_spaces() {
    spaces="$((${1:-0} / $CHARWIDTH + 1))"
    printf "% *s\n" $spaces
}

output="$(px_to_spaces `tray_width_px`)"
echo -e "padding|string|$output\n"
