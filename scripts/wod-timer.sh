#!/bin/bash

eval "$(argh.sh $@)"

sets="${sets:-1}"                    #- number of sets
rest="${rest:-5}"                    #- seconds between sets
time="${time:-60}"                   #- duration of one set
countdown="${countdown:-10}"         #- countdown before starting

rest_bg="${c_rest:-white}"           #- bg color during rest
complete_bg="${c_complete:-black}"   #- restore bg color when finished
countdown_bg="${c_countdown:-green}" #- bg color during countdown
set_odd_bg="${c_set_odd:-red}"       #- bg color during odd sets
set_even_bg="${c_set_even:-blue}"    #- bg color during even sets

rest_fg="${c_rest:-black}"           #- fg color during rest
complete_fg="${c_complete:-white}"   #- restore fg color when finished
countdown_fg="${c_countdown:-black}" #- fg color during countdown
set_odd_fg="${c_set_odd:-black}"     #- fg color during odd sets
set_even_fg="${c_set_even:-black}"   #- fg color during even sets


function clecho() {
  clear
  which banner >/dev/null && banner `echo $@` || echo $@
}

function countdown() {
  for n in $(seq 0 $1 | tac) ; do
    clecho $n
    sleep 1
  done
}

function bg() {
  printf "\e]11;$1\007"
}

function fg() {
  printf "\e]10;$1\007"
}

function color() {
  bgc="$1_bg"
  fgc="$1_fg"
  bg ${!bgc} # indirect expansion, i luv you!
  fg ${!fgc}
}

function parity() {
  [ `expr $1 % 2` == 1 ] && echo 'odd' || echo 'even'
}


color 'countdown'
countdown $countdown

for set in $(seq 1 $sets) ; do
  color "set_$(parity $set)"
  countdown $time

  if [[ $set -lt $sets ]] ; then
    color 'rest'
    countdown $rest
  fi
done

color 'complete'

