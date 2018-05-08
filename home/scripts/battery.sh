#!/bin/bash

[[ $(acpi -a) == 'Adapter 0: on-line' ]] && echo && exit
[[ -d "/sys/class/power_supply/BAT0" ]] || (echo && exit)

PWR="$(acpi -b | grep Discharging.*remaining | cut -f4 -d' ' | tr -d '?' | tr -d ',' )"
[[ "x$PWR" == "x" ]] && exit
#https://github.com/zoresvit/dotfiles/tree/master/xmonad/.xmonad/images

case "$PWR" in
  100%    ) COLOR='green'          ICON='battery-full'  ;;
  [7-9]?% ) COLOR='dimgrey'        ICON='battery-full'  ;;
  [4-6]?% ) COLOR='lightgoldenrod' ICON='battery-med'   ;;
  [2-3]?% ) COLOR='yellow'         ICON='battery-low'   ;;
  * )       COLOR='red'            ICON='battery-empty' ;;
esac

[[ ICON == 'battery-empty' ]] && notify-send "Battery alert" "$PWR" -u critical
ICON="<icon=$ICON.xbm/>"
echo "<fc=$COLOR>$ICON$PWR</fc> "


