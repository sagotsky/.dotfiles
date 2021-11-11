#!/bin/bash

# runs rubocop on behalf of vim
# will try to update bundler if necessary since ale will silently drop warnings if we don't have the gem

opts() {
    override_yml="./.rubocop.override.yml"
    if [ -f "$override_yml" ] ; then
        echo "--config $override_yml"
    else
        echo ""
    fi
}


rubocop() {
    if [ -f bin/rubocop ] ; then
        bin/rubocop "$@" `opts`
    else
        bundle exec rubocop "$@" `opts`
    fi
}

(rubocop -v || notify-send -u critical bundle "updating rubocop" && bundle install )&>/dev/null
rubocop "$@"
