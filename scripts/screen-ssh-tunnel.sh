#!/bin/bash -i

# there can be only one
pidof -x $0 | sed -e "s/$$//" | xargs kill 2>/dev/null    
killall autossh 2>/dev/null


# set up ssh tunnels to home exactly once but refresh them if they die
TUNNELS='-R 12345:127.0.0.1:22222 -R 3000:127.0.0.1:3000'
OPTIONS='-o ServerAliveInterval=60 -o ServerAliveCountMax=3 -o BatchMode=yes'
AUTOSSH_MAXSTART=2

while [[ true ]] ; do
  eval $(keychain -q --eval)
  date
  # 8080 is NATed to rj.  if it curls, we're up.  hopefully this will bypass the spam detector.
  curl -s robotjesus.net:8080 && autossh $TUNNELS $OPTIONS rj
  sleep 5m
done

# or do some signal trapping to die
