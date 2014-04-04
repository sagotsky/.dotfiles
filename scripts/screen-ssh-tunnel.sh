#!/bin/bash -i

# set up ssh tunnels to home exactly once but refresh them if they die

ssh-keyed || xterm -e 'ssh-add'


while : ; do
  autossh -R 12345:127.0.0.1:2222 -R 3000:127.0.0.1:3000 rj
  sleep 3m
done
#nohup autossh -f -R 3000:127.0.0.1:3000 rj
