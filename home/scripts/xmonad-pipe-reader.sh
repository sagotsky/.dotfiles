#!/usr/bin/env sh

# read in xmonad titles.  reformat them a bit for polybar
# tail -f "/tmp/.polybar.fifo" | sed -u \
#   -e 's/<1>/  /g' \
#   -e 's/<2>/  /g' \
#   -e 's/<3>/  /g' \
#   -e 's/<4>/  /g' \
#   -e 's/<5>/  /g' \
#   -e 's/<6>/  /g' \
#   -e 's/<7>/  /g' \
#   -e 's/<8>/  /g' \
#   -e 's/<9>/  /g' \
#   -e 's/<0>/  /g' \

tail -f "/tmp/.polybar.fifo" | sed -u \
  -e 's/<1>/⚫/g' \
  -e 's/<2>/⚫/g' \
  -e 's/<3>/⚫/g' \
  -e 's/<4>/⚫/g' \
  -e 's/<5>/⚫/g' \
  -e 's/<6>/⚫/g' \
  -e 's/<7>/⚫/g' \
  -e 's/<8>/⚫/g' \
  -e 's/<9>/⚫/g' \
  -e 's/<0>/⚫/g' \
