# restores original xterm colors.  see .bashrc.
if [ $(which xtermcontrol) -a -f ~/.xtermcontrol.bashrc -a "$XTERMCONTROL_BG" -a "$TERM" == 'xterm' ] ; then
  xtermcontrol --bg="$XTERMCONTROL_BG" 
  xtermcontrol --fg="$XTERMCONTROL_FG"
fi

# when leaving the console clear the screen to increase privacy
if [ "$SHLVL" = 1 ]; then
    [ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
fi
