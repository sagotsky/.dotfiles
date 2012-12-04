#!/bin/bash

PWR=$(acpi -b | cut -f4 -d' ' | tr -d '%' | tr -d ',')

if [ $PWR -gt 0 ] ; then
  COLOR='red'
  SYM='%'
fi

if [ $PWR -gt 20 ] ; then
  COLOR='yellow'
fi

if [ $PWR -gt 60 ] ; then
  COLOR='green'
fi

if [[ $(acpi -a) == 'Adapter 0: on-line' ]] ; then
  COLOR='cadetblue'
  PWR='AC'
  SYM=''
fi

echo "<fc=$COLOR>$PWR$SYM</fc>"

