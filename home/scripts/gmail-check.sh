#!/usr/bin/env bash

# checks all gmail accounts using firefox cookies

TEMP_SQLITE=`mktemp`
TEMP_COOKIE_JAR=`mktemp`

function cookie-jar() {
  # backup isn't locked probably.  does it have everything we need?  or do we need a temp file?
  FILE="$(find /home/sagotsky/.mozilla/firefox -maxdepth 2 -mindepth 2 -name cookies.sqlite)" #.bak
  cp $FILE $TEMP_SQLITE
}

function dump-cookies() {
  cookie-jar
  # copy pastad these.  do we need them?
  echo "# Netscape HTTP Cookie File" > $TEMP_COOKIE_JAR

  sqlite3 -separator $'\t' $TEMP_SQLITE >>$TEMP_COOKIE_JAR <<- EOF
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
  rm $TEMP_SQLITE
}

function accounts() {
  grep '/mail/u/[0-9]' $TEMP_COOKIE_JAR | awk -e '{print $3}' | sort | uniq
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

function debug() {
  [[ "$DEBUG" != '' ]] && echo "$@" 1>&2
}

dump-cookies

for segment in `accounts` ; do
  URL="https://mail.google.com/$segment/feed/atom"

  ATOM="$(curl -s -b $TEMP_COOKIE_JAR $URL)"

  debug $ATOM

  # echo $ATOM | grep Unauth && exit 1
  icon `echo "$ATOM" | grep '<entry>'`

  debug ----
done | paste -s -d\  -

