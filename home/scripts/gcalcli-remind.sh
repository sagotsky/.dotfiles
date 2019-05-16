#!/usr/bin/env bash

# continuously checks gcalcli to get active gcal events.  this is meant to feed into polybar or equivalent

# this is more complicated than it lets on.  the event will stay active for quite a while.  i'd like to be able to clear it once i've seen it.
# the solution is that this script has a daemon loop.  when you run the daemon, it also sets up a trap, which clears output and sleeps, then resumes output.
# the second option is a snooze button.  it finds the daemon's pid and send it the USR1 signal to hit the trap.

SNOOZE_DURATION=300s

reminders() {
  gcalcli remind 1 'echo %s' 2>/dev/null | head -n 1
}

snooze-mode() {
  echo ''
  sleep $SNOOZE_DURATION
}

snooze() {
  PID=$(ps ax | grep "`basename $0` [d]aemon" | cut -f 1 -d' ')
  [[ "$PID" != "" ]] && kill -s USR1 $PID
}

daemon() {
  while [[ "$SNOOZE" != '1' ]] ; do
    reminders
    sleep 1
  done
}

case $1 in
  'daemon')
    trap snooze-mode USR1
    daemon
    ;;
  'snooze')
    snooze $2
    ;;
  *)
    echo -e "Usage: \n  `basename $0` daemon - inits daemon.  \n  `basename $0` snooze - clears notifications and snoozes daemon"
esac
