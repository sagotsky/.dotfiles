#!/bin/bash -x

# update album art for currently playing song

STATE=$(cmus-remote -Q)
ALBUM=$(echo "$STATE" | grep '^tag album' | cut -f 3- -d ' ')
ARTIST=$(echo "$STATE" | grep '^tag artist' | cut -f 3- -d ' ')
FILE=$(echo "$STATE" | grep '^file' | cut -f 2- -d ' ')

COVER="${FILE%/*}/folder.jpg"

# 600px or bigger is good enough
if [[ -f "$COVER" ]] ; then
  RES=$(identify -verbose "$COVER" | grep Geometry | cut -f 4 -d ' ' | cut -f 1 -d 'x')
  #[[ "$RES" -ge 600 ]] && exit
fi

URL=$(album-art.rb "$ARTIST" "$ALBUM")
if [[ "$URL" != '' ]] ; then
  curl -s "$URL" > /tmp/folder.jpg
  file '/tmp/folder.jpg' | grep image && mv /tmp/folder.jpg "$COVER"

  CACHED="$HOME/.cmus/albumart/$(echo ${COVER/$HOME\//} | tr '/' '_')"
  [[ -f "$CACHED" ]] && rm "$CACHED"
fi

