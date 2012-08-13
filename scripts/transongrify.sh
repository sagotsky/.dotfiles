#!/bin/bash

# framework for piping together audio formats

# encoder and decoder functions.  
# decoder outputs to stdout
# encoder recieves AND outputs to stdout

PROCS=$(cat /proc/cpuinfo  | grep ^processor | wc -l) # 1 process per core
export DESTINATION='/tmp/' # ./ when mature?
export VERBOSE=1
set -a


mp3_dec() {
  mpg321 --quiet --wav - "$@"
}
mp3_enc() {
  lame --quiet -r -h -V 0 - -
}
# index gets id3 tags.  returns newline delimited list of tag value
mp3_index() {
  mp3info "$@" -p "artist %a\ngenre %g\nalbum %l\ntrack %n\ntitle %t\nyear %y\n"
}
mp3_tag() {     # recieves list of "name value\n" from stdin
  FILE="$@"
  TAGS=""
  while read line ; do
    case $line in
      artist*)  CMD='-a' ;;
      genre*)   CMD='-g' ;;
      album*)   CMD='-l' ;;
      track*)   CMD='-n' ;;
      title*)   CMD='-t' ;;
      year*)    CMD='-y' ;;

      *)        CMD='' ;;
    esac

    ARG="$(echo $line | cut -f2- -d' ' -s)"
    CMD="$CMD"

    if [[ "$ARG" != "" && "$CMD" != "" ]] ; then
      mp3info "$FILE" $CMD "$ARG"
    fi
  done
}

wav_enc() {
  sox -t .wav - -t .wav -
}
wav_dec() {
  sox "$@" -t .wav -
}

ogg_enc() {
  oggenc -Q -
}
ogg_dec() {
  oggdec -Q -o - "$@"
}
ogg_tag() {     # recieves list of "name value\n" from stdin
  FILE="$@"
  TAGS=""
  while read line ; do
    ARG="$(echo $line | cut -f2- -d' ' -s)"

    if [[ "$ARG" != '' ]] ; then
      case $line in
        artist*)  CMD="-t \"ARIST=$ARG\"" ;;
        #genre*)   CMD="-t \"GENRE=$ARG\"" ;;
        album*)   CMD="-t \"ALBUM=$ARG\"" ;;
        track*)   CMD="-t \"TRACKNUMBER=$ARG\"" ;;
        title*)   CMD="-t \"TITLE=$ARG\"" ;;
        year*)    CMD="-t \"DATE=$ARG\"" ;;

        *)        CMD='' ;;
      esac

      TAGS="$TAGS $CMD"
    fi

  done
  vorbiscomment -w "$FILE" $TAGS
}


m4a_dec() {
  faad --stdio --quiet "$@"
}
m4a_enc() {
  faac -w - -o -
}
m4a_index() {
  faad -i "$@" 2>&1 | grep '^\(title\|artist\|album\|genre\|track\)' | tr -d ':'
}
m4a_tag() {
  echo m4a
}


flac_dec() {
  flac --decode --silent --stdout "$@"
}
flac_index() {
  metaflac --list --block-type=VORBIS_COMMENT "$@"
}
# hooks
# EXT_dec() decodes a file.  wav to stdout
# EXT_env() encodes a file.  takes stdin
# EXT_index() reads out tags.  
# EXT_tag() reads in tags.  applies to file, $@.

# put each hook set in ext.module or something.  include on demand.

set +a


# notifications
usage() {
  cat <<EOF
Usage: echo \$files | transongrify.sh FORMAT

Echo file names into transongrify to convert them into FORMAT
Source formats must have a decoder function.  Destination formats
need an encoder function.  This script is really just a place to
dump preferences for encoding.

To convert current directory, try ls | transongrify.sh.  

Output is (for now) going to end up in current directory.  Eventually
I should add support for files with paths.
EOF
}

# set globals or give feedback
if [ $# -eq 1 ] ; then
  FMT=$1
  ENC="${FMT}_enc"

  if [[ $(type -t $ENC) == 'function' ]] ; then
    export FMT
    export ENC
  else
    echo "Error.  No encoder for $FMT format."
    exit
  fi

else 
  usage
  exit
fi

# call encoders and decoders
convert() {
  FILE="$@"
  SRC_EXT=${FILE##*.}  # lowercase it?
  DEC="${SRC_EXT}_dec"
  debug "Converting $FILE"

  if [[ -f $FILE && $(type -t $DEC) == 'function' ]] ; then
    DEST="$DESTINATION/${FILE%.*}.$FMT"
    $DEC "$FILE" | $ENC > "$DEST" 

    debug "Converted $FILE to $DEST"

    # try to tag the files as well
    INDEX="${SRC_EXT}_index"
    TAG="${FMT}_tag"
    if [[ $(type -t $INDEX) == 'function' && $(type -t $TAG) == 'function'  ]] ; then
      debug tagging: $INDEX "$FILE" \| $TAG "$DEST" #needs to be dest file...
      $INDEX "$FILE" | $TAG "$DEST"
    fi

  fi
}
export -f convert

# debug output
debug() {
  if [ $VERBOSE -eq 1 ] ; then
    echo $@
  fi
}
export -f debug

# run convert on all input files
while read line ; do
  echo "$line"
done | xargs -n 1 -P $PROCS -I{}  bash -c convert\ \"\{\}\"
# ugliness above quotes string to avoid paren errors

