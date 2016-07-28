#!/bin/bash

# Daemon that writes git statuses into a text file for faster shell prompt integration
# Run with no argument, this gets the git prompt for cwd
# With a path to a repo as an argument, this sets up a daemon to generate prompts

REPO=$1

repo() {
  if [[ "$REPO" == '' ]] ; then
    git rev-parse --show-toplevel 2>/dev/null
  else 
    echo $REPO
  fi
}

reponame() {
  basename $(repo)
}

tempdir() {
  repo="$(repo)"
  echo "/tmp/git-teleprompter/${repo//\//_}"
}

tempfile() {
  echo "$(tempdir)/$(reponame)"
}

daemon() {
  echo "$0 $(repo)"
}

run_daemon() {
  nohup $(daemon) &>/dev/null &
  sleep 1
}

is_running() {
  ps ax | grep -v grep | grep "$(daemon)" &>/dev/null
}

git_prompt() {
  git-super-status.sh
}

# this feels unclean to call self with arg to launch daemon
# maybe run daemon could nohup an eval of some function?
# if so, how do we detect it in is_running
if [[ "$#" == "0" ]] ; then
  is_running || run_daemon &
  cat $(tempfile)
else
  mkdir -p $(tempdir)
  echo '…' > $(tempfile)
  while [[ "$?" == '0' ]]  ; do
    inotifywait -r "$(repo)/.git/" -e MODIFY &> /dev/null
    git_prompt >  $(tempfile)
  done
fi
