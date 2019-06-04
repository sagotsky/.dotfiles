#!/usr/bin/env bash

# byebug prevents remote pry from connecting correctly.  warn devs to comment it out.
grep "^\s*gem .*pry-byebug" Gemfile &>/dev/null && echo 'comment out pry-byebug' && exit 1

while : ; do
  docker/run bundle exec pry-remote -w
done
