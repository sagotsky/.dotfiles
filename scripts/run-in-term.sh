#!/bin/bash

# launches a term running whatever args are provided
# This is stupid, but I couldn't figure out way to launch a command in a term in vim via keybaord, without waiting ofr the shell to exit

xterm -e $@ &
