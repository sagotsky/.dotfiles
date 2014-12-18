#!/bin/bash

# gmail-count.sh
# 
# Displays mail count or just a dot when you have unread mail in
# gmail.  This uses a cookie exported from firefox (see cookies exporter)
# instead of a file with your password.  

# there can be only one
pidof -x $0 | sed -e "s/$$//" | xargs kill 2>/dev/null
USER=0 # env var? 0 => blue, 1 => red

while [ -f ~/.gmail-cookies.txt ] ; do
  SLEEP='15m' # new mail check.  will be faster after mail present or errors so they clear sooner
  chmod 600 ~/.gmail-cookies.txt
  CURL=$(curl -s -b ~/.gmail-cookies.txt  https://mail.google.com/mail/u/$USER/feed/atom/)
  ERROR=$(echo $CURL | grep '<HTML>')
  MSG=''

  if [[ $ERROR != '' ]] ; then
    MSG='[?]';
    SLEEP='1m';
  else 
    COUNT=$(echo $CURL | grep '<entry>' | wc -l)

    if [[ $COUNT != '0' ]] ; then
      MSG=' •' #echo "[$COUNT]"
      SLEEP='1m';
    fi
  fi

  echo "$MSG"
  sleep $SLEEP
done
