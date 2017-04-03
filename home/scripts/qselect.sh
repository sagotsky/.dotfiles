#!/bin/bash

# replacement for bash's select.  doesn't need enter to be pressed.
# may not be suitable for use with more than 10 items.

count=0
declare -A hash

vals=`head -n10`
for val in vals ; do
  hash[$count]=$val
  echo "$count) $val"
  count=$(( count + 1 ))
done

read -s -N1 key < /dev/tty # dev tty lets us read user input even though we're in a pipe

echo "${hash[$key]}"
