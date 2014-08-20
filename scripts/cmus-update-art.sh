#!/bin/bash -x

# update album art for currently playing song

# known false positives:
#   'iron maiden' 'the x factor'
#   album/cd{1,2}
#   *&*
#   vapor trails - original gets remixed art
# buckethead - bucketheadland, enter t

# http://www.imagemagick.org/Usage/compare/
# maybe figure out average color from art.  or loop over term colors, to find the least used and make those the ones cmus gets

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
  cmus-remote -u
  cmus-remote -u
fi

