#!/bin/sh

file=$1
shift

if [ ! -f "$file" ] ; then
  touch "$file"
fi

IFS=$'\n' stdin="$(cat)"
sel=$(echo -e "$stdin\n$(grep . $file)" | sort | uniq -c | sort -bnr | sed -e 's/.*[0-9]\+ //' | dmenu $@)

echo "$sel" >> "$file"
echo "$sel"
