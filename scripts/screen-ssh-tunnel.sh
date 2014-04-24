#!/bin/bash -i

# there can be only one
pidof -x $0 | sed -e "s/$$//" | xargs kill 2>/dev/null    
killall autossh 2>/dev/null

XPID=`ps ax | grep '/usr/bin/X'`
# make this part of while?


# set up ssh tunnels to home exactly once but refresh them if they die

ssh-keyed || xterm -e 'ssh-add'
TUNNELS='-R 12345:127.0.0.1:2222 -R 3000:127.0.0.1:3000'
OPTIONS='-o ServerAliveInterval=60 -o ServerAliveCountMax=3 -o BatchMode=yes'

while : ; do
  autossh $TUNNELS $OPTIONS rj
  sleep 3m
done

# or do some signal trapping to die
