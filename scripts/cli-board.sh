#!/bin/bash

# clip board copier
# lists files in .cli-board in a dmenu
# copies contents or output (if executable) into clipboard


CLIBOARDHOME="$HOME/.cli-board/"
FONT=' -fn -*-lucida-bold-r-*-*-16-*-*-*-*-*-*-* '
DMENU_OPTS="-t -i -b -m 0 -nb #ffb -nf black -sb #aa7 -sf white -p cli-board $FONT"
export DISPLAY=:0.0

getfile() {
  DIR="$@"
  FILE=$(find $DIR -mindepth 1 -maxdepth 1 -printf '%f\n' -regex '^[^\.]' |\
    dmenu $DMENU_OPTS)

  # -d, dig into directory?
  if [ -d "$DIR/$FILE" ] ; then
    getfile "$DIR/$FILE"  
  else 
    if [ -x "$DIR/$FILE" ] ; then
      echo $($DIR/$FILE) | xsel -i
    else 
      cat "$DIR/$FILE" | xsel -i
    fi
  fi
}

getfile "$CLIBOARDHOME"
