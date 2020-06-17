#!/bin/bash

# add ssh agent env vars to all tmux sessions

tmux ls -F'#S' | while read session_name ; do
  tmux set-environment -t $session_name SSH_AUTH_SOCK ~/.ssh/ssh_auth_sock # $SSH_AUTH_SOCK
  tmux set-environment -t $session_name SSH_AGENT_PID $SSH_AGENT_PID
  tmux set-environment -t $session_name DISPLAY $DISPLAY
done
