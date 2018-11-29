#!/bin/bash

alias tmux='systemd-run --scope --user tmux'

if [[ "$1" == '-h' || "$1" == '--help' ]] ; then
  echo tmux-session.sh session_name prog1 prog2 ... progN
  exit 1
fi
SESSION=$1
shift

# Runs a script in tmux or attaches to existing
tmux ls | grep -q "$SESSION" ||
  tmux -2 new-session -d -s "$SESSION" "$1"
  shift

  while [[ "$#" -gt "0" ]] ; do
    NTH="$(( NTH + 1 ))"
    prog="${1}"
    tmux -v new-window -t "$SESSION:$NTH" "$prog"
    shift
  done

    # eval $(keychain --eval)
    # screen-ssh-tunnel.sh
