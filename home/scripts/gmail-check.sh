#!/usr/bin/bash

set -euf -o pipefail


DEBUG=${DEBUG:-''}

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
  find /home/sagotsky/.mozilla/firefox -maxdepth 2 -mindepth 2 -name $FILENAME -mtime +7 -delete

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

check-mail
