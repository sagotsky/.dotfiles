#!/usr/bin/bash

# This shim runs an executable with GDK_DPI_SCALE set to enlarge an interface.
# To use it, creating a symlink pointing to this file.  It will run an executable whose name matches.

export GDK_DPI_SCALE=1.5

APP="$(basename $0)"
THIS_SHIM_PATH="$(dirname  ${BASH_SOURCE[0]})" # remove this script from path so we don't recurse
SYMLINKED_FILE="$(readlink ${BASH_SOURCE[0]})" # check if we're calling this outside of a symlink

if [[ "$SYMLINKED_FILE" == "" ]] ; then
  # No symlink, so print usage and bail out
  echo 'Usage: create a symlink pointing to this script.  Name it after the app you want it to wrap.'
else
  PATH="${PATH//$THIS_SHIM_PATH/}" $APP
fi
