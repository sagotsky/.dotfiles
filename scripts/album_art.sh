#!/bin/bash 

# find music folders that don't have album art


set -a
function has_mp3() {
  [[ "$(find "$PWD/$@/" -maxdepth 1 -iname '*.mp3' | wc -l)" != '0' ]] && return 0
}

function has_albumart() {
return 1
  [[ "$(find "$PWD/$@/" -maxdepth 1 -iname 'folder.jpg' | wc -l)" != '0' ]] && return 0
}

function get_albumart() {
  DIR="$@"
  FILE=$(find "$DIR" -name '*.mp3' | head -n 1)
  ARTIST=$(mp3info "$FILE" -p %a | tr ' ' +)
  ALBUM=$(mp3info "$FILE" -p %l | tr ' ' +)
  
  mbid=$(curl -s "http://ws.audioscrobbler.com/2.0/?method=album.getinfo&api_key=$(cat $HOME/.lastfm)&artist=$ARTIST&album=$ALBUM&format=json" | jshon -e album -e mbid | tr -d '"')
  if [[ "$mbid" == '' ]] ; then
    ALBUM=$(fuzzy_album "$ARTIST" "$ALBUM" | tr ' ' +)
    mbid=$(curl -s "http://ws.audioscrobbler.com/2.0/?method=album.getinfo&api_key=$(cat $HOME/.lastfm)&artist=$ARTIST&album=$ALBUM&format=json" | jshon -e album -e mbid | tr -d '"')
  fi
  link=$(curl -s "http://coverartarchive.org/release/$mbid" | cut -f 2 -d' ')
  img=$(curl -s -L "$link" | jshon -e images -e 0 -e image  | tr -d '"')

  [[ "$img" != '' ]] && curl "$img" > "$DIR/folder.jpg" ; echo $ARTIST - $ALBUM
}

# when there's no clear album, do something clever
function fuzzy_album() {
  ARTIST="$1"
  ALBUM="${2//+/ }"
  ALL=$(curl -s "http://ws.audioscrobbler.com/2.0/?method=artist.gettopalbums&artist=$ARTIST&api_key=2ae0cdecdc23f5ffad8ef6c5f335258c&format=json&limit=500" | jshon -e topalbums -e album -a -e name | tr -d '"')
  echo "$ALL" | while read line ; do
    HIT=$(echo ${line//+/ } $ALBUM |  
      tr ' ' "\n" |
      sort |
      uniq -d -i | 
      wc -l
    )

    MISS=$(echo ${line//+/ } $ALBUM |  
      tr ' ' "\n" |
      sort |
      uniq -u -i | 
      wc -l
    )
    


    echo $(( ($HIT * 5) - $MISS )) $line
  done | sort -n | tail -n 1 | cut -f 2- -d' '
}

set +a

#fuzzy_album rush "moving pictures 2112 live"
#exit 

find "$@" -type d \
  -execdir bash -c 'has_mp3 "{}"' \; \
  -and -not -execdir bash -c 'has_albumart "{}"' \; \
  -print0 \
  -print | while read line ; do
    echo $line
    #get_albumart "$line"
  done


