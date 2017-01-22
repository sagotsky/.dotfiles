#!/bin/sh

# git merge-base master 

function current_branch() {
  git branch | grep '*' | cut -f2 -d' '
}

function branches_first_commit() {
  git merge-base master $1
}

BRANCH="${1:-`current_branch`}"
git log "$(branches_first_commit $BRANCH)"..HEAD --name-only --pretty=format: | sort | uniq | grep '/'



