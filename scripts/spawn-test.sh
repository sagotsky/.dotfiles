#!/bin/bash

# there can be only one
pidof -x $0 | sed -e "s/$$//" | xargs kill 2>/dev/null
PRE='xvfb-run'

TESTPATH="$HOME/repos/plm/test"
cd $TESTPATH/..
while :  ; do 
  TEST="$(inotifywait $TESTPATH --exclude '.*swp' -r -e modify | tr -s ' MODIFY ' /)"
  clear
  date
  echo $TEST
  bundle exec rake test TEST=$TEST
  # bundle exec rake test:recent
done
