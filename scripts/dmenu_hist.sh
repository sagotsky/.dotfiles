#!/bin/sh

file=$1
shift

if [ ! -f "$file" ] ; then
  touch "$file"
fi

IFS=$'\n' stdin="$(cat)"
sel=$(echo -e "$stdin\n$(cat $file)" | sort | uniq -c | sort -bnr | cut -f8- -d' ' | dmenu $@)

echo "$sel" >> "$file"
echo "$sel"
