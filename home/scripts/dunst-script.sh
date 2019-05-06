#!/bin/bash

APP=$1
SUMMARY=$2
BODY=$3
ICON=$4
URGENCY=$5

echo -e "app: $APP\nsum: $SUMMARY\nbody: $BODY\nicon: $ICON\nurgency: $URGENCY\n" >> /tmp/dunst.log

case $APP in
  'Electron'|'Slack') # slack's official client
    #flag-urgent.sh Slack &
    case $SUMMARY in
      '#robothouse') ;;
      '#jira') ;;
      'research_platform') ;;
      'sysops') ;;
      '#openresearchexchange') ;;
      *) [[ "$BODY" =~ '^github' ]] || flag-urgent.sh Slack & ;;
    esac
    ;;


  'Spotify')
    music-client.sh bandsong >> ~/.music.out &
    ;;

  'Thunderbird')
    flag-urgent.sh 'Thunderbird' &
    ;;
esac

case $SUMMARY in
  cmus)
    music-client.sh bandsong > ~/.music.out &
    ;;
esac

