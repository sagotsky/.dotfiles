#!/bin/sh

# use with dunstctl set-paused toggle
# emits true or false when state changes

dbus-monitor type=signal,member=PropertiesChanged,interface=org.freedesktop.DBus.Properties,path=/org/freedesktop/Notifications --profile | while read line ; do
    dunstctl is-paused
done
