#!/bin/bash

# applies filters to cmus based on your music directory's structure 
# requires argh and dmenu

eval "$(argh.sh $@)" 

RANDOMIZE="${RANDOMIZE:-}"                         #- Randomize selection instead of using a dmenu
LIST="${LIST:-song}"                               #- List music by tag, as defined by structure [default: song]
STRUCTURE="${STRUCTURE:-genre/artist/album/song}"  #- Directory structure for your music.  [default: genre/artist/album/song]
DIR="${DIR:-$HOME/Music/}"                         #- Location of music [default: $HOME/Music/]
QUERY="${QUERY:-}"                                 #- Search by query instead of a menu pipe.  Uses first result.

# find position of this tag in the given dir strcuture
POS=0
for tag in ${STRUCTURE//\// } ; do
  POS=$(( $POS + 1 ))
  [[ "$LIST" == "$tag" ]] && break
done

# set up find args to get this item
if [[ "$LIST" == "song" ]] ; then
  FINDARGS=" -mindepth $POS -type f -iregex .*\.\(mp3\) " # no maxdepth - some albums have cd1/cd2 subdirs
else
  FINDARGS=" -mindepth $POS -maxdepth $POS -type d" 
fi
[[ "$QUERY" != '' ]] && FINDARGS="$FINDARGS -iregex .*${QUERY// /\\s}.*" # quoting is hard.  lets swap out spaces for a regex.

# Prep our pipes.  FORMAT affects display.  SELECT runs the dmenu.
OFFSET=$(echo ${DIR//\// } | wc -w )
FORMAT="cut -f $((POS + $OFFSET + 1)) -d/"

SELECT=$( [[ "$DISPLAY" != '' ]] && echo 'dmenu -i -l 20 -b -s 0' || echo 'slmenu -i -b -l 13') 
[[ "$RANDOMIZE" != '' ]] && SELECT="shuf -n 1" 

# Get a song or dir and send it to cmus, (yes the extra find is redundant but it should be cached on hd)
PLAY="$(find $DIR $FINDARGS | $FORMAT)" 
[[ $(wc -l <<< "$PLAY") -gt 1 ]] && PLAY="$(echo "$PLAY" | sort | $SELECT )" 
FULLPATH=$(find $DIR $FINDARGS -name "$(echo $PLAY | sed -e 's/\(\[\|\]\)/\\\1/g')")

if [[ "$PLAY" != "" ]] ; then
  cmus-remote -C live-filter # not sure this is totally necessary
  cmus-remote -C "add $FULLPATH" 
  cmus-remote -C "live-filter ~f ${FULLPATH//[\(\)]/*}" 
  cmus-remote -C "echo Playing: $PLAY"
  cmus-remote -n -p
fi
