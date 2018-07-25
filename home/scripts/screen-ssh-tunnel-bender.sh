#!/bin/bash -i

# there can be only one
pidof -x $0 | sed -e "s/$$//" | xargs kill 2>/dev/null
killall autossh 2>/dev/null


# set up ssh tunnels to home exactly once but refresh them if they die
        # ssh 23456               rails 30000             rails 40000              vpn/jenkins 32333 (because 32.33 um repeating of course)
TUNNELS='-R 23456:127.0.0.1:22 -R 30000:127.0.0.1:3000 -R 40000:127.0.0.1:4000 -R  32333:10.52.2.98:80' #jenkins?

OPTIONS='-o ServerAliveInterval=60 -o ServerAliveCountMax=3 -o BatchMode=yes'
AUTOSSH_MAXSTART=2

while [[ true ]] ; do
  eval $(keychain -q --eval)
  date
  # 8080 is NATed to rj.  if it curls, we're up.  hopefully this will bypass the spam detector.
  #curl -s robotjesus.net:8080 && autossh $TUNNELS $OPTIONS rj
  autossh -M 55555 $TUNNELS $OPTIONS rj
  sleep 1m
done

# or do some signal trapping to die
