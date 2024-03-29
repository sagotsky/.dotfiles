#!/bin/sh

## WM needs
# xmonad style dual dipslay
# tiling with master window
# ewmh
# zoom
# sane extensions (dwm patches are teh worst)
# stability
# minimal dependencies.
# speed
# sane defaults
# package managed ( config.h ruins this)

# WM launcher in a loop.  xinitrc launches this in a loop.  edit and then kill wm.sh to get a new WM.

# echinus
# not tiling by default.

# exec chamfer --config=/usr/share/chamfer/config/config.py --shader-path=/usr/share/chamfer/shaders # didn'treally launch
# /usr/bin/dk # automatic mode has potential.  feels like it's off the happy path though.
# bspwm # saw similar off screen behavior as left.  could yambar be culprit?
# leftwm # 12/22/22 getting better, but multimonitor shows artifacts on secondary screen.  can't remove margin or change color still.
# qtile start -c ~/.config/qtile/config.py &> /tmp/qtile # dec 22, no windows visible.  abandoned config.  default config has same zoom weirdness as dk.  back to dk.
# python based xmonad replacement.  kinda slow.  doesn't like zoom.  doesn't like yambar plus tray
dk


# dwm
# fights with polybar.  probably solvable.
# bar height hint is ignored
# rejects workspaces.rb reading window status.
# ewmh and anybar patches seem helpful, but won't both apply.  patch based modules are dumn.
#   patches are a dealbreaker.  does it work without?
# workspace 0 is all the windows?  expose?
#   workspaces are bitwise and get ANDed together.  they're also tagged windows, not ewmh desktops
# NET_WM_DESKTOP is missing.  can't tell which desktop a window belongs to

# frankenwm
# mostly good out of the box.  fights with workspaces.sh.  unclear that dual display will ever work.

# wtftw # feature set sounds good on paper, but don't seem to work locally

# ~/source/adwm/src/adwm
# # dwm fork with better ewmh support.  too much config, but seems tameable.
# # unfocused windows get transparency settings that i can't figure out how to remove.  dealbreaker?
# # experiencing crashing around zoom.  maybe a dealbreaker.

# velox
# busted out of the box

# # frankenwm # super fast.  workspaces.rb doesn't see windows outside of what's visible.  dual display sounds iffy.

# # i3 # is it time to try again?

# # wingo
# # arch package busted.  not a total dealbreaker but def an ill omen

# pretty good!
#  workspaces.rb doesn't work.  but leftwm-state looks easier to work around.
#  fullscreen behavior is bad
#  theme won't stick
#  not sure how multimonitor will behave
#  0.2.6 broke in aur and hasn't been fixed in a week.  giving up til this is more stable.
#  yambar won't show up

# xmonad gold standard.  would love to ditch haskell though.

# spectrwm
# while : ; do spectrwm ; sleep 1 ; done # similar features to xmonad.  too laggy.  zoom was fubar.
# herbstluftwm # lag is odd.  can't tell if it's my scripts or all of hlwm.
