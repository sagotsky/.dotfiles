#!/bin/bash

if [[ "$TERM" != rxvt-* && "$TERM" != screen* ]] ; then
  echo "Your \$TERM ($TERM) does not appear to be an rxvt variant"
  exit 1
fi

eval "$(argh.sh $@)"

# change display options on a running urxvt
bg="${bg:-}"            #- Set background color
fg="${fg:-}"            #- Set foreground color
title="${title:-}"      #- Set window title
cr="${cr:-}"            #- Set cursor color
pr="${pr:-}"            #- Set pointer color
hl="${hl:-}"            #- Set highlight color
pixmap="${pixmap:-}"    #- Set background image.
fn="${fn:-}"            #- Set font (e.g. "xft:Droid Sans Mono-16")
fni="${fni:-}"          #- Set italics font (e.g. "xft:Droid Sans Mono-16")
fnb="${fnb:-}"          #- Set bold font (e.g. "xft:Droid Sans Mono-16")
fnbi="${fnbi:-}"        #- Set bold italic font (e.g. "xft:Droid Sans Mono-16")
colorit="${colorit:-}"  #- Set italic image.
colorbd="${colorbd:-}"  #- Set bold image.
colorul="${colorul:-}"  #- Set underline image.
bd="${bd:-}"            #- Set border color
# TODO: find codes for italic,bold font.  also add helper for changing face on all three


# man 7 urxvt for key codes
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
CMDS['fnb']=711
CMDS['fni']=712
CMDS['fnbi']=713

for key in "${!CMDS[@]}" ; do
  if [[ "${!key}" != '' ]] ; then
    CMD=${CMDS[${key}]}
    ESCAPE_CODE='\e]%s;%s\007'

    [[ $TERM == screen* ]] && ESCAPE_CODE="\ePtmux;${ESCAPE_CODE//\e/\e\\e}\e\\\\"
    printf $ESCAPE_CODE $CMD "${!key}"
  fi
done
