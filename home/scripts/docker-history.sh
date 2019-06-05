#!/bin/bash

FILTER_CMD="dmenu -matching fuzzy -tokenize -lines 20 -width 80 -fullscreen"
REPOS="ez-rails pos-rails"

docker-history() {
  docker/run "cat /usr/src/shared/.pry_history 2>/dev/null ; cat /root/.local/share/pry/pry_history 2>/dev/null"
}

script_name="$(basename $0)"
cache_dir="$HOME/.cache/${script_name%.sh}"
mkdir -p "$cache_dir"

/usr/bin/cat $cache_dir/* | $FILTER_CMD |xsel

repo='pos-rails'
for repo in $REPOS ; do
  cd "$HOME/repos/$repo"
  docker-history > "$cache_dir/$repo"
done
