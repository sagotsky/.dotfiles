#!/bin/sh

# track a new remote branch
git branch --track "$@" remotes/origin/"$@"
