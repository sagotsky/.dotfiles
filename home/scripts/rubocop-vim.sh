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
# no opts temporarily
opts() {
    echo ""
}


# notify-send debug "running rubocop-vim"
exec_rubocop() {
    if [ -f "docker/run-q" ] ; then
        notify-send debug "docker/run-q bundle exec rubocop $@ `opts`" # bin/rubocop loops
        docker/run-q bin/rubocop "$@" `opts`
    elif [ -f bin/rubocop ] ; then
        notify-send debug "bin"
        bin/rubocop "$@" `opts`
    else
        notify-send debug "bundle"
        bundle exec rubocop "$@" `opts`
    fi
}

# (rubocop -v || notify-send bundle "updating rubocop" && bundle install )&>/dev/null
exec_rubocop "$@"
notify-send debug "done"
