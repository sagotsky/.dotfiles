#!/bin/sh

while true
do
	if [ -S ~/.awesome_ctl.0 ]; then
	    while true
	    do 
		echo "0 widget_tell mystatusbar clock text   `   date +\"%A, %b %d - %I:%M \"` "
		echo "" # empty line flushes awesome text
		sleep 10
	    done | awesome-client
	else
	    sleep 1
	fi
done
