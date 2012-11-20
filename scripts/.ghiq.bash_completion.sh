#!/bin/bash

# bash completion for ghiq.py

# git config --get remote.origin.url

_ghiq() {
  local cur prev cli_opts dir
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  prev="${COMP_WORDS[COMP_CWORD-1]}"
  dir="$HOME/.ghiq.cache/"

  get_repo() {
    for val in "${COMP_WORDS}" ; do
      lastval=$val
      if [[ "$lastval" == '--repo' || "$lastval" == '-r' ]] ; then
        #also make sure it exists
        echo $val
        return 0 
      fi
    done

    repo=$(git config --get remote.origin.url | tr '/' ':' | cut -f 2 -d:)
    echo $repo
  }

  cli_opts="--repo --label --user"

  repo_opts() {
    find "$dir" -type f -printf "%f \n"
  }

  label_opts() {
    # figure out the repo
    # works once.  gets lost once partial strings show up.

    repo=$(get_repo)
    echo $repo
    labels=$(grep '^labels:' $dir/$repo | cut -d: -f2- | tr ' ' '_')
    echo $labels 
  }

  case ${prev} in
    #--label) echo "${COMP_WORDS[@]}" ; return 1 ;;
    --repo) COMPREPLY=( $(compgen -W "$(repo_opts)" -- ${cur}) ) ; return 0 ;;
    --label) COMPREPLY=( $(compgen -W "$(label_opts)" -- ${cur}) ) ; return 0 ;;
  esac

  # alternatively, just have one comp reply line and change opts?
  if [[ ${cur} == -* ]] ; then
    COMPREPLY=( $(compgen -W "${cli_opts}" -- ${cur}) )
    return 0
  fi

}
complete -F _ghiq ghiq.py
