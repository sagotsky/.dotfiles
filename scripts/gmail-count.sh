#!/bin/sh

# gmail-count.sh
# 
# Displays mail count or just a dot when you have unread mail in
# gmail.  This uses a cookie exported from firefox (see cookies exporter)
# instead of a file with your password.  

while [ -f ~/.gmail-cookies.txt ] ; do
  chmod 400 ~/.gmail-cookies.txt
  CURL=$(curl -s -b ~/.gmail-cookies.txt  https://mail.google.com/mail/feed/atom/)
  ERROR=$(echo $CURL | grep '<HTML>')

  if [[ $ERROR != '' ]] ; then
    echo [?];
  else 
    COUNT=$(echo $CURL | grep '<entry>' | wc -l)

    if [[ $COUNT != '0' ]] ; then
      echo ' •' #echo "[$COUNT]"
    fi
  fi

  sleep 15m
done
