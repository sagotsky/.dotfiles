# restores original xterm colors.  see .bashrc.
if [ $(which xtermcontrol) -a -f ~/.xtermcontrol -a "$XTERMCONTROL_BG" -a "$TERM" == 'xterm' ] ; then
  xtermcontrol --bg="$XTERMCONTROL_BG" --file=/dev/null
  xtermcontrol --fg="$XTERMCONTROL_FG" --file=/dev/null
fi
