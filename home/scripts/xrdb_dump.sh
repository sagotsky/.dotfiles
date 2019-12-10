#!/bin/bash

# Reads out *properties from xrdb.  Returns bash to set them as shell vars
# Usage: eval $(xrdb_dump.sh)

export_cmd() {
  echo export $1="$2"
}

xrdb_vars() {
  DISPLAY=$(cat ~/.display) xrdb -query |
    grep '^*' |
    sed -e 's/\*\./xrdb_/' |
    tr -d \:
}

uname | grep "Darwin" &>/dev/null && exit

xrdb_vars | while read line ; do export_cmd $line ; done
