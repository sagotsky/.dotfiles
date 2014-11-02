#!/bin/bash

# Create a symlink to this file.  Its name must match a steam app's folder name.
# Running the script will cd into that folder and find then run the game launcher

TARGET=$(basename "$0")
# different behaviorif we're running steam_shim on its own?  maybe create a shim?
STEAM="$HOME/.steam/SteamApps/common/"
cd "$STEAM/$TARGET"
pidof steam || (steam &>/dev/null & sleep 10s)

find ./ -maxdepth 1 -mindepth 1 -executable | while read bin ; do
  [[ "$(file $bin)" =~ 'script' ]] && $bin
done
