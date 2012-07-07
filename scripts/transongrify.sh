#!/bin/bash

# framework for piping together audio formats

# encoder and decoder functions.  
# decoder outputs to stdout
# encoder recieves AND outputs to stdout

PROCS=4 # 1 process at a time
set -a


mp3_dec() {
  mpg321 --quiet --wav - "$@"
}
mp3_enc() {
  lame --quiet -r -h -V 0 - -
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

hook_get_meta() {
  echo 'write me'
}

hook_tag() {
  echo 'me too'
  #array arg?
}

hook_dependencies() {
  echo 'bin bin2 etc'
}

# put each hook set in ext.module or something.  include on demand.

set +a


# notifications
usage() {
  cat <<EOF
Usage: echo \$files | transongrify.sh FORMAT

Echo file names into transongrify to transmute them into FORMAT
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
transmute() {
  FILE="$@"
  SRC_EXT=${FILE##*.}  # lowercase it?
  DEC="${SRC_EXT}_dec"
  if [[ -f $FILE && $(type -t $DEC) == 'function' ]] ; then
    echo $FILE 
    $DEC "$FILE" | $ENC > "/tmp/${FILE%.*}.$FMT" 

    # tagging hooks
  fi
}
export -f transmute

# run transmute on all input files
while read line ; do
  echo "$line"
done | xargs -n 1 -P $PROCS -I{}  bash -c transmute\ \"\{\}\"
# ugliness above quotes string to avoid paren errors

