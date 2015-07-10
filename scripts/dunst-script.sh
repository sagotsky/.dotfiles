#!/bin/bash

APP=$1
SUMMARY=$2
BODY=$3
ICON=$4
URGENCY=$5

case $SUMMARY in
  cmus) 
    music-client.sh bandsong > ~/.music.out &
    ;;

# is scudcloud app or summary?  notify send has me so confused.
esac

