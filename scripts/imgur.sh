#!/bin/bash -x

# Send image files to imgur

CONFPATH="$HOME/.imgup.conf"
AUTH_TOKEN='x'
REFRESH_TOKEN='x'
BROWSER="${BROWSER:=x-www-browser}"

# rewrite stored token.  call with no args to clear it
function update_tokens() {
  AUTH_TOKEN="$1"
  REFRESH_TOKEN="$2"
  cat > $CONFPATH <<-EOF
# Conf file for $CONFPATH
export APP_ID="c0a8d35f2378812"
export APP_SECRET="60be9fe195fb2360256644af2b8a5fcf7ea6a92e"
export AUTH_TOKEN="$AUTH_TOKEN"
export REFRESH_TOKEN="$REFRESH_TOKEN"
EOF
}

function auth_pin() {
  url="https://api.imgur.com/oauth2/authorize?client_id=${APP_ID}&response_type=pin&state=auth"
  $BROWSER "$url" &>/dev/null &
  pin=$(zenity --entry --title 'Imgur Authentication' --text="Please log into imgur, grant access, and paste the pin code here.  \n$url")

  #read pin
  response=$(curl -s -d "client_id=${APP_ID}&client_secret=${APP_SECRET}&grant_type=pin&pin=${pin}" https://api.imgur.com/oauth2/token)
  if [[ "$response" == *Invalid* ]] ; then
    echo 'Invalid pin' 1>&2
    exit 
  fi

  echo $response
}

# try to auth.  uses latest token, refresh token, and finally requests pin
function auth() {
  AUTH_TOKEN=$1
  REFRESH_TOKEN=$2

  # auth with current token.  if they work, keep them
  response=`curl -s -X GET https://api.imgur.com/3/account/me/albums/ --header "Authorization: Bearer ${AUTH_TOKEN}"` # actually just gets albums.  using this to ping
  #if [[ "$response" =~ '"status":200' ]] ; then
  if [[ $(echo $response | jshon -e status) == 200 ]] ; then
    echo $AUTH_TOKEN $REFRESH_TOKEN 
    return
  fi

  # try refreshing
  response=`curl -s -d "client_id=${APP_ID}&client_secret=${APP_SECRET}&grant_type=refresh_token&refresh_token=${REFRESH_TOKEN}"  https://api.imgur.com/oauth2/token`

  # enter pin
  if [[ $(echo $response | jshon -Q -e success) == 'false' ]] ; then
    response=$(auth_pin)
  fi

  AUTH_TOKEN=$(echo $response | jshon -e access_token)
  REFRESH_TOKEN=$(echo $response | jshon -e refresh_token)

  echo $AUTH_TOKEN $REFRESH_TOKEN 
}

function upload_image() {
  image="$1"
  echo "Uploading $image..." | dzen2 -p -ta l&
  PID=$!

  response=`curl -s -X POST --header "Authorization: Bearer ${AUTH_TOKEN}" -F "image=@${image}" https://api.imgur.com/3/image`
  if [[ "$(echo $response | jshon -e status)" == '200' ]] ; then
    echo $response | jshon -e data -e link | tr -d '"\\' 
  else 
    echo "error uploading $image"  1>&2
  fi 

  kill $PID
}

# check dependecies
for dep in zenity jshon ; do
  which $dep &>/dev/null
  if [ $? -gt 0 ] ; then
    echo Missing dependency: $dep 1>&2
    exit 1
  fi
done

# write a default imgup.conf
[[ -f "$CONFPATH" ]] ||  update_tokens
source $CONFPATH

# try to auth.  if there's a change in refresh tokens, rewrite conf file
AUTH_RESPONSE=$(auth $AUTH_TOKEN $REFRESH_TOKEN)
if [[ "$AUTH_RESPONSE" != "$AUTH_TOKEN $REFRESH_TOKEN" ]] ; then
  update_tokens $AUTH_RESPONSE
  source ${CONFPATH}
fi

if [[ $# -gt 0 ]] ; then
  for image in $@ ; do
    upload_image $image
  done 
else 
  while read image ; do
    upload_image $image
  done
fi 
