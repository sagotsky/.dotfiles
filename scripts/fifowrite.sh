#! /bin/sh

# makes $1 a fifo.  writes the rest into that fifo
# this exists because python is retarded about writing to a fifo 
# that's not being read.

FIFO=$1
shift
echo "$@" >> $FIFO &

