#!/bin/bash

TITLE=' '
reminders() {
  gcalcli remind 10 "notify-send -u critical $TITLE '$(reminder_info)'"
}

date_range() {
  echo "$( date +%H:%M ) $( date -d "+ 15 minutes" +%H:%M )"
}

reminder_info() {
  gcalcli --nocolor agenda --detail location `date_range` | grep '.'
}

# zoom_link() {
#   gcalcli --nocolor agenda --detail description --detail location |
#     grep 'ezcater.zoom' |
#     head -n 1 |
#     sed -e 's/.*\(https[a-z0-9\/:\/]*\)/\1/'
# }


# min      hour day_of_month month week_day
#  14,29,44,59  *  *  *  *  DISPLAY=`cat $HOME/.display` DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus" notify-send gcalcli-notify.sh foo */
# *  *  *  *  *  DISPLAY=`cat $HOME/.display` DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(grep sagotsky /etc/passwd | cut -f 3 -d:)/bus" /home/sagotsky/scripts/gcalcli-notify.sh
reminders
