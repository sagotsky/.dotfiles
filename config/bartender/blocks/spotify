#!/bin/bash

killall playerctl &>/dev/null


# todo: fix when pulse is killed
while which playerctl &>/dev/null ; do
  playerctl -p spotify metadata -f '{{ status }}{{ title }}' --follow 2>/dev/null | while read status ; do
    case $status in
      Playing*)
        playerctl metadata -f "{{ artist }} - {{ title }}"
        ;;
      Paused*)
        echo "-"
        ;;
      **)
        echo ""
        ;;
    esac
  done
  sleep 10
done
