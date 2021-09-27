#!/bin/bash

# runs rubocop on behalf of vim
# will try to update bundler if necessary since ale will silently drop warnings if we don't have the gem

(bin/rubocop -v || notify-send bundle "updating rubocop" && bundle install )&>/dev/null
bin/rubocop "$@"
