#!/bin/sh

# common use for find.  hopefully I'll get this memorized soon and won't have to script it.
# finds files bigger than 10mb, prints kb and name, sorts by size
nice find  / -size +10M -printf "%k kb\t%p\n" 2> /dev/null | sort -n # %k is size in kb, %p is file with path
