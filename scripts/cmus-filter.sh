#!/bin/bash

# applies filters to cmus based on your music directory's structure 
# requires docopts and dmenu
eval "$(docopts -A args -V - -h - : "$@" <<EOF
Usage: cmus_select.sh [--list <tag>] [--random] [--structure=</alt/dir/structure>] [--dir=/path/to/music]

  -h --help                Show help
  -r --random              Randomize selection instead of using a dmenu
  -l --list TAG            List music by tag.  Unless you use -s, genre, artist, album, and song are expected. [default: song]
  -s --structure STRUCT    Directory structure for your music.  [default: genre/artist/album/song]
  -d --dir DIR             Location of music [default: $HOME/Music/]
----
cmus_select.sh 0.0.1
EOF
)"

# find position of this tag in the given dir strcuture
for tag in ${args['--structure']//\// } ; do
  POS=$(( $POS + 1 ))
  [[ "${args['--list']}" == "$tag" ]] && break
done

# set up find args to get this item
if [[ "${args['--list']}" == "song" ]] ; then
  FINDARGS=" -mindepth $POS -type f -iregex .*\.\(mp3\) " # no maxdepth - some albums have cd1/cd2 subdirs
else
  FINDARGS=" -mindepth $POS -maxdepth $POS -type d" 
  CAP='/*' 
fi

# Prep our pipes.  FORMAT affects display.  SELECT runs the dmenu.
OFFSET=$(echo ${args['--dir']//\// } | wc -w )
FORMAT="cut -f $((POS + $OFFSET + 1)) -d/"
SELECT="dmenu -i -l 20 -b -s 0"
[[ "${args['--random']}" == 'true' ]] && SELECT="shuf -n 1" 

# Get a song or dir and send it to cmus
DIR=$(find $HOME/Music/ $FINDARGS | $FORMAT | sort | $SELECT )
if [[ "$DIR" != "" ]] ; then
  cmus-remote -C "live-filter ~f */$(echo $DIR | tr '()' '*')$CAP" # cmus treats parens as grouping.  
  cmus-remote -C "echo Playing: $DIR"
  cmus-remote -n -p
fi
