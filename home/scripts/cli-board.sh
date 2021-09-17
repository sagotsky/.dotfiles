#!/bin/bash

# clip board copier
# lists files in .cli-board in a dmenu
# copies contents or output (if executable) into clipboard


CLIBOARDHOME="$HOME/.cli-board/"
# FONT=' -fn -*-terminus-bold-r-*-*-16-*-*-*-*-*-*-* '
# DMENU_OPTS="-i -nb #3b653d -nf #fff -sb #3b653d -sf #fe4 $FONT"

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
    if [ -x "$DIR/$FILE" ] ; then
      echo $($DIR/$FILE) | xsel-all.sh
    else
      cat "$DIR/$FILE" | xsel-all.sh
    fi
  fi
}

getfile "$CLIBOARDHOME"
