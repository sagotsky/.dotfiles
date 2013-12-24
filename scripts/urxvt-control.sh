#!/bin/bash

# change display options on a running urxvt
# requires docopts
eval "$(docopts -A args -V - -h - : "$@" <<EOF
Usage: urxvt-control.sh [--bg <bg>] [--fg <fg>] [--title <title>] [--cr <cr>] [--pr <pr>] [--hl <hl>] [--pixmap <pixmap>] [--fn <fn>] [--colorit=<colorit>] [--colorbd=<colorbd>] [--colorul=<colorul>] [--bd <bd>]

  --bg=<bg>    Set background color
  --fg=<fg>    Set foreground color
  --title=<title>   Set window title
  --cr=<cr>   Set cursor color
  --pr=<pr>   Set pointer color
  --hl=<hl>   Set highlight color
  --pixmap=<pixmap>   Set background image.
  --fn=<fn>   Set font (e.g. "xft:Droid Sans Mono-16")
  --colorit=<colorit>   Set italic image.
  --colorbd=<colorbd>   Set bold image.
  --colorul=<colorul>   Set underline image.
  --bd=<bd>   Set border color

----
urxvt-control.sh 0.1
EOF
)"

# see: http://www.freebsd.org/cgi/man.cgi?query=urxvt&manpath=ports&sektion=7
declare -A CMDS
CMDS['title']=2
CMDS['fg']=10
CMDS['bg']=11
CMDS['cr']=12
CMDS['pr']=13
CMDS['hl']=17
CMDS['pixmap']=20
CMDS['fn']=50
CMDS['colorit']=704
CMDS['colorbd']=706
CMDS['colorul']=707
CMDS['bd']=708
CMDS['b']=709

for key in "${!args[@]}" ; do
  if [[ "${args[$key]}" != '' ]] ; then
    CMD=${CMDS[${key##--}]}
    printf '\33]%s;%s\007' $CMD "${args[$key]}"
  fi
done
