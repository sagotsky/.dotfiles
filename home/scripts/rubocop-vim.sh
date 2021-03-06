#!/bin/bash

# runs rubocop on behalf of vim
# will try to update bundler if necessary since ale will silently drop warnings if we don't have the gem

# BUNDLE_CMD="bundle"
# if [[ "$DOCKER_SPECS" != "" ]] ; then
# need all gems to be local?
#   BUNDLE_CMD="docker/run bundle"
# fi
# if [[ -f "bin/rubocop" ]] ; then
#   docker/run bin/rubocop -a $@
#   [[ "$?" != "0" ]] && docker/run bundle exec gem install rubocop rubocop  && docker/run bin/rubocop -a $@
# else
#   rubocop -a $@
# fi

# NO DOCKER!   JUST INSTALL BUNDLE INSTALL
# ez-specific.
# if [[ -f "docker/run" ]] ; then
#   cmd="docker-compose exec -T ez-rails-web sh -c bin/rubocop $@"
#   # export COMPOSE_INTERACTIVE_NO_CLI=0
#   echo $cmd
#   `$cmd`
#   # COMPOSE_INTERACTIVE_NO_CLI=0 docker-compose exec -T ez-rails-web sh -c "bin/rubocop " $@ " "
#   # can we keep the args?  -a and friends are stripped
# else
#   rubocop -a $@
# fi


(bin/rubocop -v || bundle install )&>/dev/null

RUBOCOP_OVERRIDE="./.rubocop.override.yml"
if [[ -f "$RUBOCOP_OVERRIDE" ]] ; then
  OPTS="--config $RUBOCOP_OVERRIDE"
fi
OPTS="${OPTS} --cache true --display-cop-names"

# bin/rubocop $@ $OPTS --display-cop-names
RUBOCOP_DAEMON_USE_BUNDLER=true BUNDLE_GEMFILE=Gemfile.local bundle exec rubocop-daemon exec -- $@ $OPTS
if [[ "$?" == "11" ]] ; then
  BUNDLE_GEMFILE=Gemfile.local bundle install
  abeep
  notify-send "rubocop" "is out of date" -u critical
fi
