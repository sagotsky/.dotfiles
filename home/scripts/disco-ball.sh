#!/bin/sh

# flash colors to kill burnout maybe

colors() {
  cat <<EOF
    #f00
    #0f0
    #00f
    #ff0
    #f0f
    #0ff
EOF
}

black_and_white() {
  cat <<EOF
    #fff
    #000
EOF
}

reset_bg_fg() {
  xtermcontrol --fg $RESET_FG --bg $RESET_BG
  exit
}

RESET_BG="$(xtermcontrol --get-bg)"
RESET_FG="$(xtermcontrol --get-fg)"

trap reset_bg_fg TERM INT

while : ; do
  color="$(colors | shuf -n 1)"
  xtermcontrol --bg $color --fg $color
  sleep 0.1

  bw="$(black_and_white | shuf -n 1)"
  xtermcontrol --bg $bw --fg $bw
  sleep 0.1
done

# catch ctrl-c
# xtermcontrol --bg black --fg white
