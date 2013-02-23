#!/bin/sh

# Dumps $DISPLAY into a a file.  
# Meant to be run when X starts so I know which display to use.

echo $DISPLAY > ~/.display
