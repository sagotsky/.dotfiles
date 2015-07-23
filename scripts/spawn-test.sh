#!/bin/bash

# there can be only one
~/scripts/there-can-be-only-one.sh
PRE='xvfb-run'
export DISPLAY="$(cat ~/.display)"

TESTPATH="$PWD/test"
cd $TESTPATH/..

while : ; do 
  TEST="$(inotifywait $TESTPATH --exclude '.*swp' -r -e modify | tr -s ' MODIFY ' /)"
  clear
  date
  echo $TEST
  #bundle exec rake test TEST=$TEST
  bundle exec rake test TEST=$TEST
  # bundle exec rake test:recent
  echo -e "\a"
done
