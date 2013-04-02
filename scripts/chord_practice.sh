#!/bin/sh

# chord_practice.sh
# Helps train your ear to various chord progressions.  
#   1. Run the program
# ` 2. listen to the chords 
#   3. guess the key and progression.
#   4. Hit enter to see if you were correct
#
# More keys and progressions can be added below
# More types of chord and scale (both are currently limited to major and minor) can be
# added in the scale() and chord() functions

# Add more keys and progressions here to increase difficulty
KEYS=$(cat <<EOF
  F major
  C major
  A major
EOF
)

PROGRESSIONS=$(cat <<EOF
  I IV V
  vi IV I V
  I V vi V
EOF
)

# globals.  bash 4 can't export arrays, so use global strings and arrayify them on demand
_NOTES="C Db D Eb E F Gb G Ab A Bb B"
export _NOTES


# given a value and an array (or str representation of an array), return the position of that value
function _indexOf() {
  val=$1
  shift
  array=($@)

  for i in $(seq 0 ${#array[@]}) ; do
    if [ ${array[$i]} == $val ] ; then return=$i ; break ; fi
  done

  echo $return
}

# Given a note, return the note $interval semitones up
function _interval() {
  note=$1
  int=$2
  declare -a notes=($_NOTES)

  index=$(_indexOf $note ${notes[@]})
  return_index=$(( ($index + $int) % ${#notes[@]} ))

  echo ${notes[$return_index]}
}

# Given a note, return notes of all named intervals
function intervals() {
  note=$1
  shift

  declare -A named_intervals
  named_intervals[perfect_unison]=0
  named_intervals[minor_second]=1
  named_intervals[major_second]=2
  named_intervals[minor_third]=3
  named_intervals[major_third]=4
  named_intervals[pefect_fourth]=5
  named_intervals[tritone]=6
  named_intervals[pefect_fifth]=7
  named_intervals[minor_sixth]=8
  named_intervals[major_sixth]=9
  named_intervals[minor_seventh]=10
  named_intervals[major_seventh]=11
  named_intervals[perfect_octave]=12

  for name in "$@" ; do
    _interval $note ${named_intervals[$name]}
  done
}

# returns the notes of a chord
function chord() {
  note=$1
  type=$2

  declare -A chord_defs
  chord_defs[major]="perfect_unison major_third pefect_fifth"
  chord_defs[minor]="perfect_unison minor_third pefect_fifth"

  intervals $note ${chord_defs[$type]}
}

# given a key and scale type, returns the notes in the scale
function scale() {
  key=$1
  tonality=$2
  declare -a notes=($_NOTES)

  # shift notes
  pos=$(_indexOf $key $_NOTES)
  len=${#notes[@]}
  notes=(${notes[@]:$pos:$len} ${notes[@]:0:$(($pos))})

  # available scales
  declare -A scale_defs
  scale_defs[minor]='2 1 2 2 1 2 2'
  scale_defs[major]='2 2 1 2 2 2 1'

  c=0
  for i in 0 ${scale_defs[$tonality]} ; do
    c=$(( ($c + $i) % $len ))
    echo ${notes[$c]}
  done
}

# given a key and part of a nashville progression, return the chord
function nashville() {
  key=$1
  tonality=$2
  item=$3
  type=''
  scale=($(scale $key $tonality))

  declare -A ROMAN
  ROMAN=([I]=1 [II]=2 [III]=3 [IV]=4 [V]=5 [VI]=6 [VII]=7)

  # major
  int=${ROMAN[$item]}
  if [ "$int" != '' ] ; then
    type='major'
    interval=$int
  fi

  # minor
  if [ "$type" == '' ] ; then
    upper=$(echo $item | tr  [:lower:] [:upper:])
    int=${ROMAN[$upper]}
    if [ "$int" != '0' ] ; then
      type='minor'
      interval=$int
    fi
  fi

  if [ "$type" != '' ] ; then
    echo ${scale[$(( $interval - 1))]} $type
  else
    echo Invalid nashville progression $item 1>&2
    exit 1
  fi
}

# returns chords from a progression.  $1 is a key.  $2... is any number of chords in nashville notation.
function progression() {
  key=$1
  tonality=$2
  shift ; shift

  for item in $@ ; do
    nashville $key $tonality $item
  done
}

# just plays some notes.  tries to kill them afterwards, otherwise alsa cris
function play_notes() {
  for note in $@ ; do
    play -qn synth pluck $note 0 0.5 &
    pids="$pids $!"
    sleep .001
  done
  sleep 1
  kill $pids &>/dev/null
}


# choose a random key and progression, then play them
my_key=$(echo "$KEYS" | shuf -n 1)
my_progression=$(echo "$PROGRESSIONS" | shuf -n 1)

progression $my_key $my_progression | while read line ; do
  notes=$(chord $line)
  play_notes $notes
done

echo 'Hit <enter> to see what was just played'
read line
echo $my_key \| $my_progression

