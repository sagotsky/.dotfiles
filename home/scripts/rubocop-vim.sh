#!/bin/bash

# runs rubocop on behalf of vim
# will try to update bundler if necessary since ale will silently drop warnings if we don't have the gem

rubocop() {
    if [ -f bin/rubocop ] ; then
        bin/rubocop "$@"
    else
        bundle exec rubocop "$@"
    fi
}

(rubocop -v || notify-send -u critical bundle "updating rubocop" && bundle install )&>/dev/null
rubocop "$@"
