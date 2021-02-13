#!/bin/bash

# Replacement for spaces :-P

# Groups windows.  Call this with group name as an arg to activate all those windows.
# Usage: use spark (or similar) to bind keys to actionscript: do shell script "~/scripts/osx-window-summoner.sh 1"

declare -a SPACES
# SPACES[1]='Chrome'
SPACES[1]='Firefox'
SPACES[2]='Spotify'
# SPACES[3]='Mail'
SPACES[4]='iTerm'
SPACES[5]='Slack'
SPACES[6]='Messages'

space="${SPACES[$1]}"

for app in $space ; do
  osascript -i <<-EOF
    tell application "$app"
      activate
    end tell
EOF
done
