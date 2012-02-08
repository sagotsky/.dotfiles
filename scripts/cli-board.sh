#!/bin/bash

# clip board copier
# lists files in .cli-board in a dmenu
# copies contents or output (if executable) into clipboard


DIR="$HOME/.cli-board/"
FONT=' -fn -*-lucida-bold-r-*-*-16-*-*-*-*-*-*-* '
DMENU_OPTS="-t -i -b -m 0 -nb #ffb -nf black -sb #aa7 -sf white -p cli-board $FONT"
FILE=$(find $DIR -type f -printf '%f\n'  | dmenu $DMENU_OPTS)
export DISPLAY=:0.0

# -d, dig into directory?
if [ -x "$DIR/$FILE" ] ; then
  echo $($DIR/$FILE) | xsel -i
else 
  cat "$DIR/$FILE" | xsel -i
fi

