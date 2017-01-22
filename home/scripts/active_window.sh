#!/bin/bash

# gets name of active window
# opens appropriate manpage term

# should handle gui windows, programs running in terms, and even programs running in screen

function getWinID() {
   xprop -root | grep "_NET_ACTIVE_WINDOW(WINDOW)" | grep -v "CUT_BUFFER0" | cut -d' ' -f5
}
function getWinTitle() {
    xprop -id $(getWinID) | grep "WM_CLASS(STRING)" | sed -e 's/.*= "\([^"]*\)".*/\1/'
}

function getWinPID() {
    xprop -id $(getWinID) | grep "NET_WM_PID(CARDINAL)" | cut -d' ' -f 3
}

# if the title is a shell, term, or screen this digs out the children
function exhume() {
    _TITLE=$TITLE
    _PID=$PID

    TITLE=$1
    PID=$2
    COUNT=$3

    #echo -e "exhuming:\t $1 $2"
    if [[ $_PID -eq $PID && $COUNT -gt 5 ]] ; then
        #echo "pid repeat or count over 5"
        echo ""
    else
        case $TITLE in 
            xterm*|rxvt*|x-terminal-emulator)
            TITLE=$(ps --ppid $PID -o comm= | tail -n 1)
            PID=$(ps --ppid $PID -o pid= | tail -n 1)
            exhume $TITLE $PID $(( $COUNT + 1 ))
            ;;

            bash)
            PROCS=$(ps --ppid $PID -o pid= | wc -w)
            #echo "Does bash have children?  ... $PROCS"
            if [[ $PROCS -gt 0 ]] ; then
                TITLE=$(ps --ppid $PID -o comm= | tail -n 1)
                PID=$(ps --ppid $PID -o pid= | tail -n 1)
                exhume $TITLE $PID $(( $COUNT + 1 ))
            fi
            ;;

            screen)
            TITLE=$(xprop -id $(getWinID) | grep "WM_NAME(STRING)" | sed -e 's/.*<\(.*\)>.*/\1/')
            # assumes hard status includes name of program in angle braces
            # we can also add the number to hardstatus and then grab the nth pid.  assumes no screens were destoryed and recreated becasue their children would have a higher pid.

            # screen is esp tricky because the screen pid you get may not be the original session.  a screen that attaches elsewhere has no hcildren.
            TITLE='screen'
            ;;

            Navigator)
            TITLE="firefox"
            ;;
# ssh breaks this as well. involves connecting to another system, so probably not worth scripting around.
        esac
    fi
}

TITLE=$(getWinTitle)
PID=$(getWinPID)

exhume $TITLE $PID 0
echo -n $TITLE
