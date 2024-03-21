#!/usr/bin/env bash

# external_screen="DP-1-1.1"
# internal_screen="eDP-1"

notify-send "docking" -h string:fgcolor:#ffffff -h string:bgcolor:#00b373

# ~/.screenlayout/vertical-2.sh || ~/.screenlayout/single.sh
~/.screenlayout/triple.sh || ~/.screenlayout/single.sh

xmodmap ~/.xmodmap
# killall yambar
# killall xcompmgr &>/dev/null
# killall trayer

# `xrandr -q` lags for 3s.  this tries to skip the query by offering a fallback.
# xrandr --output "$external_screen" --primary ||                     # try external
#     xrandr --output "$internal_screen" --primary       # fallback to laptop

dkcmd restart
xbacklight-50
CAPS_UNLOCK.SH # just in case I hit caps before xmodmap
~/.xsession.d/xset.sh &>/dev/null &
nitrogen --restore
# ~/.xsession.d/xcompmgr &>/dev/null &
# yambar &
