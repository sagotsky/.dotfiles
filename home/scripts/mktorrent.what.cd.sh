#!/bin/sh

# makes torrents for what.cd

ANNOUNCE="http://tracker.what.cd:34000/9r40f3c2pihyve2wa54v1b3ebq0fxdfq/announce"
~/bin/mktorrent -a $ANNOUNCE -p "$@"
