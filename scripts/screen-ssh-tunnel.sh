#!/bin/bash

# set up ssh tunnels to home exactly once but refresh them if they die

function mk_tunnel() {
  echo tunneling
  while true ; do ssh -R 12345:127.0.0.1:2222 rj ; sleep 3m ; done
}

set -a
function do_ssh() {
  echo doing ssh: $@
  while true ; do ssh -R $@ ; done
}

screen -ls | grep tunnels || (
  screen -d -m -S tunnels 
  while read tunnel ; do
    screen -S tunnels -X screen bash -c do_ssh\ "$tunnel"
  done <<EOF
    12345:127.0.0.1:2222 rj
    3000:127.0.0.1:3000 rj
EOF
)
