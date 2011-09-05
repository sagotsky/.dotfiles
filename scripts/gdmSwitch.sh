#!/bin/bash

# Takes an arg, switches to that user's gdm session or starts a new one
# if none is open

# needs to be -u user, -?h help
# needs option for locking

#gdmflexiserver


# bail if too many instances running
#COUNT=$(pidof -x gdmSwitch.sh | wc -w)
#if [ $COUNT -gt 2 ] ; then 
	#echo "`date` bailing - too many gdms" >> ~sagotsky/.gdmswitcher;
	#exit 1 
#fi

# bail if too many instances running
if [[ $$ -ne $(pidof -x gdmSwitch.sh) ]] ; then
	exit 1
fi

function usage() { sed -e 's/^  //' <<EndUsage

  gdmSwitch.sh usage:
  gdmSwitch.sh takes exactly one argument, which is a username.  If that username
  has an open xsession, switches to it.  This is kinda like a console version of
  gnome-switcher-applet except that (for now) it doesn't let you query for users
  and pick one.

EndUsage
  exit 1
}

if [ $# -ne 1 ] ; then
    usage
else
	date >> .gdmswitcher

    NAME=$1
    VT=$(gdmflexiserver -c CONSOLE_SERVERS | sed -e "s/^.*$NAME,\([0-9]*\).*$/\1/")
    
    ##check if we found a term or if the whole string didn't match by doing a character count
    N=$(echo "$VT" | wc -c)
    
    if [ $N -gt 2 ] ; then
	## no open session
		rhythmbox-client --pause --no-start # otherwise pulseaudio is likely to crash
		gdmflexiserver -a -l -c "FLEXI_XSERVER" > /dev/null
    else 
		rhythmbox-client --pause --no-start # otherwise pulseaudio is likely to crash
		# switch to open session
		gdmflexiserver -a -l -c "SET_VT $VT" >  /dev/null
    fi
fi

# gdm commands:
# -a authenticate
# -l leave unlocked
# -c command
# -c FLEXI_XSERVER login screen
# -c SET_VT goes to virtual term #


