#!/usr/bin/bash

set -euf -o pipefail

DEBUG=${DEBUG:-''}
EMPTY_SNOOZE=${EMPTY_SNOOZE:-0}  # if mail is empty, sleep for some seconds.  no snooze if its full so it can check when its empty sooner.

# checks all gmail accounts using firefox cookies

function dump-cookies() {
  # copy pastad these.  do we need them?
  echo "# Netscape HTTP Cookie File"

  sqlite3 -separator $'\t' "$@" <<- EOF
.mode
.header off
    select
      host,
      case substr(host,1,1)='.' when 0 then 'FALSE' else 'TRUE' end,
      path,
      case isSecure when 0 then 'FALSE' else 'TRUE' end,
      expiry,
      name,
      value

    from moz_cookies
    where host like '%google.com'
    ;
EOF
  rm
}

# find all cookie jars
# delete adjacent dumps if old
# copy jar
# make dump
# delete spare

function firefox-cookie-jars() {
  FILENAME='cookies-gmail.txt'

  # delete after a week
  find /home/sagotsky/.mozilla/firefox -maxdepth 2 -mindepth 2 -name $FILENAME -mtime +1 -delete

  for orig_sql in "$(find /home/sagotsky/.mozilla/firefox -maxdepth 2 -mindepth 2 -name cookies.sqlite)" ; do
    profile_dir="${orig_sql%/*}"
    cookies_txt="$profile_dir/$FILENAME"
    tmp_sql="$profile_dir/cookies-tmp.sqlite"

    if [ ! -f "$cookies_txt" ] ; then
      cp $orig_sql $tmp_sql
      dump-cookies $tmp_sql > $cookies_txt
      rm "$tmp_sql"
    fi

    echo $cookies_txt
  done
}


function accounts() {
  for file in $(firefox-cookie-jars) ; do
    grep '/mail/u/[0-9]' $file | awk -e '{print $3}' | sort | uniq
  done
}

function debug() {
  [[ "$DEBUG" != '' ]] && echo "$@" 1>&2 || true
}

function icon() {
  ICON=""
  COLOR=''

  if [[ "$@" == *"Inbox for sagotsky@gmail.com"* ]] ; then
    COLOR="#5484ed"
  fi

  if [[ "$@" == *"Inbox for jon.sagotsky@ezcater.com"* ]] ; then
    COLOR="#88bb40"
  fi

  debug $COLOR
  [[ "$COLOR" != "" ]] && echo "%{F$COLOR}$ICON%{F-}"
}

function curl-cookie-opt() {
  firefox-cookie-jars | while read file ; do
    echo -n "-b $file "
  done
}

function check-mail() {
  for segment in `accounts` ; do
    URL="https://mail.google.com/$segment/feed/atom"


    ATOM="$(curl -s $(curl-cookie-opt) $URL)"

    debug $ATOM

    # echo $ATOM | grep Unauth && exit 1 # todo - unauthed path
    icon `echo "$ATOM" | grep '<entry>'` || true

    debug ----
  done | paste -s -d\  -
}

function new-mail-urls() {
  for segment in `accounts` ; do
    URL="https://mail.google.com/$segment/feed/atom"


    ATOM="$(curl -s $(curl-cookie-opt) $URL)"

    debug $ATOM

    if [[ "$ATOM" =~ '<entry>' ]] ; then
      echo "${URL%feed/atom}"
    fi

    debug ----
  done | paste -s -d\  -
}

# todo: firefox || icons
if [[ "${1-}" == 'url' ]] ; then
  new-mail-urls
else
  mail=$(check-mail)
  [ "$(echo $mail | grep [a-Z] | wc -c)" == "1" ] && sleep ${EMPTY_SNOOZE:-0}
  echo $mail
fi



# unauthed content.  delete cache and retry?
# <HTML> <HEAD> <TITLE>Unauthorized</TITLE> </HEAD> <BODY BGCOLOR="#FFFFFF" TEXT="#000000"> <H1>Unauthorized</H1> <H2>Error 401</H2> </BODY> </HTML>
# ----
# <?xml version="1.0" encoding="UTF-8"?><feed version="0.3" xmlns="http://purl.org/atom/ns#"><title>Gmail - Inbox for sagotsky@gmail.com</title><tagline>New messages in your Gmail Inbox</tagline><fullcount>0</fullcount><link rel="alternate" href="https://mail.google.com/mail/u/1" type="text/html"/><modified>2019-06-06T14:48:33Z</modified></feed>
# ----


