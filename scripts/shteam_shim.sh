#!/bin/bash 

# Create a symlink to this file.  Its name must match a steam app's folder name.
# Running the script will cd into that folder and find then run the game launcher

TARGET=$(basename "$0")
shteam.rb exec $TARGET
