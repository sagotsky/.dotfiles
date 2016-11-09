#!/bin/bash

APP=$1
SUMMARY=$2
BODY=$3
ICON=$4
URGENCY=$5


case $APP in
  'Electron') # slack's official client
    #flag-urgent.sh Slack & 
    case $SUMMARY in
      '#robothouse') ;;
      '#jira') ;;
      'research_platform') ;;
      'sysops') ;;
      'openresearchexchange') ;;
      *) [[ "$BODY" =~ '^github' ]] || flag-urgent.sh Slack & ;;
    esac
    ;;

  'ScudCloud Slack_SSB')
    case $SUMMARY in
      '#robothouse') ;;
      '#jira') ;;
      *)
        #printf '%14s| %s\n' "$SUMMARY" "$BODY" >> ~/.scudcloud.log
        flag-urgent.sh scudcloud &
        ;;
    esac
  ;;

  'Spotify') 
    music-client.sh bandsong >> /tmp/dunst.log
    music-client.sh bandsong > ~/.music.out &
    ;;
esac

case $SUMMARY in
  cmus) 
    music-client.sh bandsong > ~/.music.out &
    ;;

# is scudcloud app or summary?  notify send has me so confused.
esac

