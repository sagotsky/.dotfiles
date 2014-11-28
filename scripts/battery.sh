#!/bin/bash  -x

if [[ $(acpi -a) == 'Adapter 0: on-line' ]] ; then
  echo
  exit 
fi 

PWR="$(acpi -b | cut -f4 -d' ' | tr -d '%' | tr -d ',')"
[[ "x$PWR" == "x" ]] && exit
#https://github.com/zoresvit/dotfiles/tree/master/xmonad/.xmonad/images

case "$PWR" in
  [7-9]* ) COLOR='dimgrey' ;;
  [4-6]* ) COLOR='lightgoldenrod' ;;
  [2-3]* ) COLOR='yellow' ;;
  * )      COLOR='red' ;;
esac

ICON='<icon=/usr/share/dzen2/bitmaps/battery.xbm/>'
SYM='%'

echo "<fc=$COLOR>$ICON $PWR$SYM</fc> "


