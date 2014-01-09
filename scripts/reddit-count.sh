#!/bin/sh

# reddit-count.sh
# 

# there can be only one
pidof -x $0 | sed -e "s/$$//" | xargs kill &>/dev/null

while [ -f ~/.reddit.json ] ; do
  SLEEP='15m' # new mail check.  will be faster after mail present or errors so they clear sooner
  MSG=' '
  CURL=$(curl -s $(cat ~/.reddit.json) | jshon -e data -e children -l)
  (( $CURL > 0 )) && MSG=' •' || SLEEP='1m'
  echo $MSG
  sleep $SLEEP
done
