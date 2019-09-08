#!/bin/bash

# runs rubocop on behalf of vim
# will try to update bundler if necessary since ale will silently drop warnings if we don't have the gem

# BUNDLE_CMD="bundle"
# if [[ "$DOCKER_SPECS" != "" ]] ; then
# need all gems to be local?
#   BUNDLE_CMD="docker/run bundle"
# fi

if [[ -f "bin/rubocop" ]] ; then
  bin/rubocop $@
  [[ "$?" != "0" ]] && bundle exec gem install && bin/rubocop $@
else
  rubocop $@
fi
