#!/bin/bash

pianokey() {
  echo -n "$1" > ~/.config/pianobar/ctl
}

case "${1//-/}" in
  play)   pianokey p ;;
  next)   pianokey n ;;
  stop)   pianokey S ;;
  status) pianokey i ;;

esac

