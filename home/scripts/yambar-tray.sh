#!/bin/sh

pkill stalonetray
pkill trayer

# launch a tray.
icon_size=23
icon_slots=2
screen_width=1920

echo "padding|string| $(printf '% *s' $icon_slots)"
echo

sleep 0.1 # make sure yambar starts first so we can draw on tpo of it

left_offset=$(($screen_width - $icon_slots * $icon_size))

# why you freeze bar?
# echo stalonetray \
#     --icon-size $icon_size \
#     --geometry "${icon_slots}x1+${left_offset}+0" \
#     --sticky \
#     --skip-taskbar \
#     --grow-gravity E \
#     --icon-gravity E \
#     --parent-bg \
#     &> /dev/null

trayer --edge top --align right --height 20 --expand true  --transparent true --alpha 220 --width 44  --widthtype pixel  &>/dev/null
