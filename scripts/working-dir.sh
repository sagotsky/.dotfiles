#!/bin/bash

# That's why they call me, they call me the working dir!
# dmenu for launching/creating tmux ide session in git work dirs

repos="$HOME/repos/"
branch="$(ls $repos/.workdirs | dmenu)"
dir="$repos/.workdirs/$branch"

function create-workdir() {
  target_repo="$(ls $repos | dmenu)"
  if [[ "$target_repo" != '' ]] ; then
    git-new-workdir "$repos/$target_repo" "$dir"
    cd $_
    git co -b $branch
  else 
    exit 
  fi 
}
[[ -d "$dir" ]] || create-workdir  

cd $dir
# how to port?
tmux-session.sh $branch 'while true ; do bundle exec rails s ; done' vim 'bundle exec rails c' $SHELL
xterm -e tmux attach
