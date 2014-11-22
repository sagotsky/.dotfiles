#!/bin/bash  -x

PWR="$(acpi -b | cut -f4 -d' ' | tr -d '%' | tr -d ',')"
[[ "x$PWR" == "x" ]] && exit

if [[ "$PWR" -gt 0 ]] ; then
  COLOR='red'
  ICON='<icon=/usr/share/dzen2/bitmaps/battery.xbm/>'
  SYM='%'
fi

if [[ "$PWR" -gt 20 ]] ; then
  COLOR='yellow'
fi

if [[ "$PWR" -gt 60 ]] ; then
  COLOR='grey'
fi

if [[ $(acpi -a) == 'Adapter 0: on-line' ]] ; then
  echo
else 
  echo "<fc=$COLOR>$ICON $PWR$SYM</fc> "
fi


