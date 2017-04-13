#!/bin/bash

# stupid xmobar script for noticing when arch needs upgrades

COUNT="$(pacman -Qu | wc -l)"

if (( $COUNT > 1 )) ; then
  echo "<fc=steelblue>[ $COUNT ]</fc>"
fi
