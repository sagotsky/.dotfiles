#!/bin/bash

APP=$1
SUMMARY=$2
BODY=$3
ICON=$4
URGENCY=$5

case $APP in
  'ScudCloud Slack_SSB')
    case $SUMMARY in
      '#robothouse')
        ;;
      *)
        flag-urgent.sh scudcloud &
        ;;
    esac
esac

case $SUMMARY in
  cmus) 
    music-client.sh bandsong > ~/.music.out &
    ;;

# is scudcloud app or summary?  notify send has me so confused.
esac

