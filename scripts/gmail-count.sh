#!/bin/sh

# gmail-count.sh
# 
# Displays mail count or just a dot when you have unread mail in
# gmail.  This uses a cookie exported from firefox (see cookies exporter)
# instead of a file with your password.  

echo
while [ -f ~/.gmail-cookies.txt ] ; do
  chmod 400 ~/.gmail-cookies.txt
  CURL=$(curl -s -b ~/.gmail-cookies.txt  https://mail.google.com/mail/feed/atom/)
  ERROR=$(echo $CURL | grep '<HTML>')
  MSG=''

  if [[ $ERROR != '' ]] ; then
    MSG='[?]';
  else 
    COUNT=$(echo $CURL | grep '<entry>' | wc -l)

    if [[ $COUNT != '0' ]] ; then
      MSG=' •' #echo "[$COUNT]"
    fi
  fi

  echo $MSG
  sleep 1m
done
