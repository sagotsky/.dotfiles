#!/bin/bash 

# Find the process that spawned this one.  Kill all 
# other instances of it.  Intended for processes that
# are started automatically, but only need one instance.

PCMD="$(ps -ocommand= -p $PPID)"
pidof -x "${PCMD##*/}" -o "$PPID" | xargs kill -9 &> /dev/null
