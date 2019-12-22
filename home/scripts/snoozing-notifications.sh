#!/bin/bash -

ps --no-headers u $(pidof dunst) |
  awk '{print $8}' |
  grep Tl &>/dev/null # Tl = snooze.  Sl = running
