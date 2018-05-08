#!/bin/bash

# cheat sheet viewer.  navigates through dir full of cheat sheets.  displays in dmenu.

CHEATSHEETS="$HOME/.cheat-sheets/"
FONT=' -fn -*-terminus-bold-r-*-*-16-*-*-*-*-*-*-* '
DMENU_OPTS="-i -nb #3b3d45 -nf #fff -sb #3b3d45 -sf #fe4 $FONT"

if [ -f "$HOME/.display" ] ; then
  export DISPLAY=$(cat "$HOME/.display")
fi

getfile() {
  DIR="$@"
  REL="${DIR#${CLIBOARDHOME}}"

  # ____d token -> dir.  ____f (or anything else) just gets stripped
  FILE=$(find $DIR -mindepth 1 -maxdepth 1 -printf '%f____%y\n' -regex '^[^\.]' |\
    sed -e 's/____d$/\//' |\
    sed -e 's/____.$//' |\
    dmenu $DMENU_OPTS -p cli-board:$REL)

  if [ "$FILE" == "" ] ; then
    exit 1
  fi

  if [ -d "$DIR/$FILE" ] ; then
    getfile "$DIR/$FILE"
  else
    sed -e 's/^$/ /' "$DIR/$FILE" | dmenu $DMENU_OPTS -l 25 | xsel -i
  fi
}

getfile "$CHEATSHEETS"
