#!/bin/bash

# switch to a user or make a new login screen

if [ $# != 1 ] ; then
  echo 'Specify a user'
  exit 1
fi

USER=$1
SESSION=$(w | grep "^$USER.*gdm-session" )

if [ "$SESSION" == "" ] ; then
  gdmflexiserver 
else
  TTY=$(echo $SESSION | cut -f 2 -d ' ' | tr -d '[:alpha:]' )
  sudo chvt $TTY
fi
