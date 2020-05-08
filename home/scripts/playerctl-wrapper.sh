#!/bin/sh

there-can-be-only-one.sh

playerctl metadata -f '{{ status }}{{ title }}' --follow 2>/dev/null | while read status ; do
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
