#!/bin/sh

### Usage: docopts.sh
###
### Options:
###   --help -h     Show help
###   --version -v  Show version

help=$(grep "^### " "$0" | cut -c 5-)
version=$(grep "^## "  "$0" | cut -c 4-)
eval "$(docopts -h "$help" -V "$version" : "$@")"

for arg in "${argv[@]}"; do
  echo "$arg"
done
